open Core_kernel
open Bap.Std
open Bap_traces.Std
open Monads.Std

module SM = Monad.State
open SM.Monad_infix

type 'a u = 'a Bil.Result.u [@@warning "-D"]
type event = Trace.event

let stub = fun _ -> SM.return ()

class context trace =
 object(self:'s)
    inherit Bili.context [@@warning "-D"]
    val events = Trace.read_events trace
    method next_event = match Seq.next events with
      | None -> None
      | Some (ev,evs) -> Some ({<events = evs; >}, ev)

    method with_events trace = {<events = Trace.read_events trace >}
  end
[@@warning "-D"]

let data_size mv =
  Word.bitwidth (Move.data mv) |> Size.of_int_opt

let mem_of_arch arch =
  let (module T : Target) = target_of_arch arch in
  T.CPU.mem

class ['a] t arch =
  let mem_var = mem_of_arch arch in
  let mem = Bil.var mem_var in
  let endian = Arch.endian arch in
  object(self)
    constraint 'a = #context
    inherit ['a] Bili.t as super [@@warning "-D"]

    method eval_memory_store mv =
      match data_size mv with
      | None -> SM.return ()
      | Some size ->
        let addr = Bil.int (Move.cell mv) in
        let data = Bil.int (Move.data mv) in
        let exp = Bil.store ~mem ~addr data endian size in
        self#eval_exp exp >>= fun r ->
        SM.update (fun c -> c#update mem_var r)

    method eval_register_write mv =
      self#eval_move (Move.cell mv) (Bil.int (Move.data mv))

    method eval_pc_update addr =
      SM.update (fun c -> c#with_pc (Bil.Imm addr))

    method eval_memory_load mv =
      match data_size mv with
      | None -> SM.return ()
      | Some size ->
        let addr = Bil.int (Move.cell mv) in
        let exp = Bil.load ~mem ~addr endian size in
        self#eval_exp exp >>| ignore

    method eval_register_read mv =
      self#lookup (Move.cell mv) >>| ignore

    method eval_exn exn =
      self#eval_cpuexn (Exn.number exn)

    method eval_event ev =
      Value.Match.(
        select @@
        case Event.memory_store self#eval_memory_store @@
        case Event.memory_load self#eval_memory_load @@
        case Event.register_read self#eval_register_read @@
        case Event.register_write self#eval_register_write @@
        case Event.code_exec self#eval_exec @@
        case Event.mode self#eval_mode @@
        case Event.pc_update self#eval_pc_update @@
        case Event.context_switch self#eval_context_switch @@
        case Event.syscall self#eval_syscall @@
        case Event.exn self#eval_exn @@
        case Event.call self#eval_call @@
        case Event.return self#eval_return @@
        case Event.modload self#eval_modload @@
        default SM.return) ev

    method eval_trace trace : 'a u =
      SM.update (fun ctxt -> ctxt#with_events trace) >>= fun () -> self#run

    method private run : 'a u =
      SM.get () >>= fun ctxt ->
      match ctxt#next_event with
      | None -> SM.return ()
      | Some (ctxt, ev) ->
        SM.put ctxt >>= fun () -> self#eval_event ev >>= fun () -> self#run

    method eval_exec: chunk -> 'a u = stub
    method eval_context_switch: int -> 'a u = stub
    method eval_mode: Mode.t -> 'a u = stub
    method eval_syscall: syscall -> 'a u = stub
    method eval_call: call -> 'a u = stub
    method eval_return: return -> 'a u = stub
    method eval_modload: modload -> 'a u = stub

  end
