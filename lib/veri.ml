open Core_kernel
open Bap.Std
open Regular.Std
open Bap_traces.Std
open Bap_future.Std
open Bap_core_theory
open Monads.Std

module Dis = Disasm_expert.Basic

module SM = Monad.State
open SM.Monad_infix

type event = Trace.event [@@deriving bin_io, compare, sexp]
type 'a u = 'a Bil.Result.u [@@warning "-D"]
type 'a r = 'a Bil.Result.r [@@warning "-D"]
type 'a e = (event option, 'a) SM.t
type error = Veri_error.t

let create_move_event tag cell' data' =
  Value.create tag Move.({cell = cell'; data = data';})

let find cond tag evs =
  let open Option in
  List.find evs ~f:(fun ev -> match Value.get tag ev with
      | None -> false
      | Some mv -> cond mv) >>| fun ev -> Value.get_exn tag ev

let create_mem_store = create_move_event Event.memory_store
let create_mem_load  = create_move_event Event.memory_load
let create_reg_read  = create_move_event Event.register_read
let create_reg_write = create_move_event Event.register_write
let value = Bil.Result.value [@warning "-D"]
let same_value x y =
  Bil.Result.Id.equal (Bil.Result.id x) (Bil.Result.id y)
[@@warning "-D"]

module Disasm = struct
  module Dis = Disasm_expert.Basic
  open Dis
  type t = (asm, kinds) Dis.t

  let insn dis mem =
    match Dis.insn_of_mem dis mem with
    | Error er -> Error (`Disasm_error er)
    | Ok r -> match r with
      | mem', Some insn, `finished -> Ok (mem',insn)
      | _, None, _ ->
        let er = Error.of_string "nothing was disasmed" in
        Error (`Disasm_error er)
      | _, _, `left _ -> Error `Overloaded_chunk

  let insn_name = Dis.Insn.name
end

module Events = Value.Set

class context stat policy trace = object(self:'s)
  inherit Veri_traci.context trace as super
  val events = Events.empty
  val stream = Stream.create ()
  val error : error option = None
  val other : 's option = None
  val insn  : string option = None
  val code  : Chunk.t option = None
  val stat  : Veri_stat.t = stat
  val bil   : bil = []

  method private make_report data =
    Veri_report.create ~bil ~data
      ~right:(Set.to_list self#events)
      ~left:(Set.to_list (Option.value_exn other)#events)
      ~insn:(Option.value_exn insn)
      ~code:(Option.value_exn code |> Chunk.data)

  method private finish_step stat =
    let s = {< other = None; error = None; insn = None; bil = [];
               stat = stat; events = Events.empty; code = None; >} in
    s#drop_pc

  method merge =
    match error with
    | Some er -> self#finish_step (Veri_stat.notify stat er)
    | None -> match insn with
      | None -> self#finish_step stat
      | Some name ->
        let other = Option.value_exn self#other in
        let events, events' = other#events, self#events in
        match Veri_policy.denied policy name events events' with
        | [] -> self#finish_step (Veri_stat.success stat name)
        | results ->
          let report = self#make_report results in
          Signal.send (snd stream) report;
          self#finish_step (Veri_stat.failbil stat name)

  method split =
    let s = {< other = Some self; events = Events.empty; >} in
    s#drop_pc

  method code  = code
  method stat  = stat
  method other = other
  method events  = events
  method reports = fst stream
  method set_bil  b = {< bil = b >}
  method set_code c = {< code = Some c >}
  method set_insn s = {< insn = Some s >}
  method notify_error er   = {< error = Some er >}
  method register_event ev = {< events = Set.add events ev; >}
  method save s  = {< other = Some s >}
  method switch  = (Option.value_exn other)#save self
  method drop_pc = self#with_pc Bil.Bot
end


let new_insn arch mem insn =
  let open KB.Syntax in
  KB.Object.create Theory.Program.cls >>= fun code ->
  KB.provide Arch.slot code arch >>= fun () ->
  KB.provide Memory.slot code (Some mem) >>= fun () ->
  KB.provide Dis.Insn.slot code (Some insn) >>| fun () ->
  code

let lift arch mem insn =
  match KB.run Theory.Program.cls (new_insn arch mem insn) KB.empty with
  | Ok (code,_) ->
    let insn = KB.Value.get Theory.Semantics.slot code in
    Ok (Insn.bil insn)
  | Error c ->
    let er = Error.of_string (Sexp.to_string (KB.sexp_of_conflict c)) in
    Error er

let target_info arch =
  let module Target = (val target_of_arch arch) in
  Target.CPU.mem, lift arch

let memory_of_chunk endian chunk =
  Bigstring.of_string (Chunk.data chunk) |>
  Memory.create endian (Chunk.addr chunk)

let other_events c = match c#other with
  | None -> []
  | Some c -> Set.to_list c#events

(*let is_previous_mv tag test ev =
  match Value.get tag ev with
  | None -> false
  | Some mv -> Move.cell mv = test

  let is_previous_write = is_previous_mv Event.register_write
  let is_previous_store = is_previous_mv Event.memory_store*)
let self_events c = Set.to_list c#events
let same_var var mv =
  String.equal (Var.name var) (Var.name @@ Move.cell mv)

let same_addr addr mv = Addr.equal addr (Move.cell mv)

type find = [ `Addr of addr | `Var of var ]

class ['a] t arch dis =
  let endian = Arch.endian arch in
  let mem_var, lift = target_info arch in

  object(self)
    constraint 'a = #context
    inherit ['a] Veri_traci.t arch as super

    method private update_event ev =
      SM.update (fun c -> c#register_event ev)

    (** [find_value x] - return a value bound with [x] where
        [x] is either address or variable. In each variant of x
        an appropriative lookup order is applied.
        A register_read/memory_load event will be emited. *)
    method private find_value x =
      let find_data ctxt cond = function
        | `Write, tag -> find cond tag (self_events ctxt)
        | `Read, tag ->
          match find cond tag (self_events ctxt) with
          | None -> find cond tag (other_events ctxt)
          | x -> x in
      let find cond make_event tags =
        SM.get () >>= fun ctxt ->
        List.find_map ~f:(find_data ctxt cond) tags |> function
        | None -> SM.return None
        | Some mv ->
          self#update_event
            (make_event (Move.cell mv) (Move.data mv)) >>= fun () ->
          SM.return (Some (Move.data mv)) in
      match x with
      | `Var var ->
        find (same_var var) create_reg_read
          [ `Write, Event.register_write; `Read, Event.register_read ]
      | `Addr addr ->
        find (same_addr addr) create_mem_load
          [ `Write, Event.memory_store; `Read, Event.memory_load; ]

    method! lookup var =
      SM.get () >>= fun ctxt ->
      self#find_value (`Var var) >>= function
      | None -> super#lookup var
      | Some data -> self#eval_exp (Bil.int data)

    method! update var result =
      self#lookup var >>= fun old ->
      if same_value old result then SM.return ()
      else super#update var result >>= fun () ->
        match value result with
        | Bil.Imm data ->
          if not (Var.is_virtual var) then
            self#update_event (create_reg_write var data)
          else SM.return ()
        | Bil.Mem _ | Bil.Bot -> SM.return ()

    method! eval_store ~mem ~addr data endian size =
      self#eval_exp addr >>= fun addr_r ->
      self#eval_exp data >>= fun data_r ->
      match value addr_r, value data_r with
      | Bil.Imm got_addr, Bil.Imm got_data ->
        self#update_event (create_mem_store got_addr got_data) >>= fun () ->
        super#eval_store ~mem ~addr data endian size
      | _ -> super#eval_store ~mem ~addr data endian size

    method! eval_load ~mem ~addr endian size =
      self#eval_exp addr >>= fun addr_res ->
      match value addr_res with
      | Bil.Bot | Bil.Mem _ -> super#eval_load ~mem ~addr endian size
      | Bil.Imm addr' ->
        self#find_value (`Addr addr') >>= function
        | Some data ->
          super#eval_store ~mem ~addr (Bil.int data) endian size >>= fun sr ->
          self#update mem_var sr >>= fun () ->
          super#eval_load ~mem ~addr endian size
        | None ->
          super#eval_load ~mem ~addr endian size >>= fun r ->
          match value r with
          | Bil.Imm data ->
            self#update_event (create_mem_load addr' data) >>= fun () ->
            SM.return r
          | _ -> SM.return r

    method private add_pc_update =
      SM.get () >>= fun ctxt ->
      match ctxt#pc with
      | Bil.Mem _ | Bil.Bot -> SM.return ()
      | Bil.Imm pc ->
        let pc_ev = Value.create Event.pc_update pc in
        self#update_event pc_ev

    method! eval_jmp addr : 'a u =
      super#eval_jmp addr >>= fun () ->
      self#add_pc_update >>= fun () ->
      SM.update (fun c -> c#switch) >>= fun () ->
      self#add_pc_update >>= fun () ->
      SM.update (fun c -> c#switch)

    method! eval bil =
      super#eval (Stmt.normalize ~normalize_exp:true bil)

    method private eval_insn (mem, insn) =
      let name = Disasm.insn_name insn in
      SM.update (fun c -> c#set_insn name) >>= fun () ->
      match lift mem insn with
      | Error er ->
        SM.update (fun c -> c#notify_error (`Lifter_error (name, er)))
      | Ok bil ->
        SM.update (fun c -> c#set_bil bil) >>= fun () ->
        self#eval bil

    method private eval_chunk chunk =
      self#update_event (Value.create Event.pc_update (Chunk.addr chunk)) >>= fun () ->
      match memory_of_chunk endian chunk with
      | Error er -> SM.update (fun c -> c#notify_error (`Damaged_chunk er))
      | Ok mem ->
        match Disasm.insn dis mem with
        | Error er -> SM.update (fun c -> c#notify_error er)
        | Ok insn -> self#eval_insn insn

    method! eval_memory_load mv =
      let addr = Bil.int @@ Move.cell mv in
      let data = Move.data mv in
      match Size.of_int_opt @@ Word.bitwidth data with
      | None -> SM.return ()
      | Some size ->
        SM.get () >>= fun ctxt ->
        super#eval_store
          ~mem:(Bil.var mem_var) ~addr (Bil.int data) endian size >>= fun r ->
        SM.put ctxt >>= fun () ->
        super#update mem_var r >>= fun () ->
        super#eval_memory_load mv

    method! eval_pc_update addr =
      super#eval_pc_update addr >>= fun () ->
      self#verify_frame

    method! eval_exec code =
      super#eval_exec code >>= fun () ->
      SM.update (fun c -> c#set_code code)

    method! eval_event ev =
      super#eval_event ev >>= fun () ->
      Value.Match.(
        select @@
        case Event.code_exec    (fun _ -> SM.return ()) @@
        case Event.memory_store (fun _ -> SM.return ()) @@
        case Event.memory_load  (fun _ -> SM.return ()) @@
        default (fun () -> self#update_event ev)) ev

    method private verify_frame : 'a u =
      SM.get () >>= fun ctxt ->
      match ctxt#code with
      | None -> SM.return ()
      | Some code ->
        SM.update (fun c -> c#split) >>= fun () ->
        self#eval_chunk code      >>= fun () ->
        SM.update (fun c -> c#merge) >>= fun () ->
        SM.return ()

    method! eval_trace trace =
      super#eval_trace trace >>= fun () -> self#verify_frame

    method! eval_while ~cond ~body =
      super#eval_exp cond >>| value >>= function
      | Imm r when Word.(r = b1) -> self#eval body
      | _ ->  SM.return ()
  end
