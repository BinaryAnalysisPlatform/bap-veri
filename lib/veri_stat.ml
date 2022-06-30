open Core_kernel[@@warning "-D"]
open Regular.Std

module Calls = String.Map

type ok_er = int * int [@@deriving bin_io, compare, sexp]

type t = {
  calls : ok_er Calls.t;
  errors: Veri_error.t list;
} [@@deriving bin_io, compare, sexp]

type stat = t [@@deriving bin_io, compare, sexp]

let empty = { calls = Calls.empty; errors = []; }
let errors t = t.errors
let notify t er = {t with errors = er :: t.errors }

let update t name ~ok ~er =
  {t with
   calls =
     Map.change t.calls name
       ~f:(function
           | None -> Some (ok, er)
           | Some (ok',er') -> Some (ok + ok', er + er')) }

let failbil t name = update t name ~ok:0 ~er:1
let success t name = update t name ~ok:1 ~er:0

module Abs = struct

  type nonrec t = t -> int

  let fold_calls {calls} f = Map.fold ~f ~init:0 calls

  let errors_count t =
    let rec loop ((ovr, dmg, undis, misl) as acc) = function
      | [] -> acc
      | hd :: tl -> match hd with
        | `Overloaded_chunk -> loop (ovr + 1, dmg, undis, misl) tl
        | `Damaged_chunk  _ -> loop (ovr, dmg + 1, undis, misl) tl
        | `Disasm_error   _ -> loop (ovr, dmg, undis + 1, misl) tl
        | `Lifter_error   _ -> loop (ovr, dmg, undis, misl + 1) tl in
    loop (0,0,0,0) t.errors

  let overloaded  t = let x,_,_,_ = errors_count t in x
  let damaged     t = let _,x,_,_ = errors_count t in x
  let undisasmed  t = let _,_,x,_ = errors_count t in x
  let mislifted   t = let _,_,_,x = errors_count t in x
  let successed   t = fold_calls t (fun ~key ~data cnt -> cnt + fst data)
  let misexecuted t = fold_calls t (fun ~key ~data cnt -> cnt + snd data)

  let abs_successed t =
    fold_calls t (fun ~key ~data cnt ->
        if snd data <> 0 then cnt
        else cnt + fst data)

  let abs_misexecuted t =
    fold_calls t (fun ~key ~data cnt ->
        if fst data <> 0 then cnt
        else cnt + snd data)

  let total t =
    List.length t.errors +
    fold_calls t (fun ~key ~data cnt -> cnt + fst data + snd data)

end

module Rel = struct
  type t = ?as_percents:bool -> stat -> float

  let apply f b t  =
    let r = float (f t) /. float (Abs.total t) in
    if b then r *. 100.0
    else r

  let ( @@ ) = apply

  let successed       ?(as_percents=false) = Abs.successed @@ as_percents
  let abs_successed   ?(as_percents=false) = Abs.abs_successed @@ as_percents
  let misexecuted     ?(as_percents=false) = Abs.misexecuted @@ as_percents
  let abs_misexecuted ?(as_percents=false) = Abs.abs_misexecuted @@ as_percents
  let overloaded      ?(as_percents=false) = Abs.overloaded @@ as_percents
  let damaged         ?(as_percents=false) = Abs.damaged @@ as_percents
  let undisasmed      ?(as_percents=false) = Abs.undisasmed @@ as_percents
  let mislifted       ?(as_percents=false) = Abs.mislifted @@ as_percents
end

module Names = struct

  type nonrec t = t -> string list

  let fold_calls ~condition t =
    Map.fold ~f:(fun ~key ~data names ->
        if condition data then Set.add names key
        else names) ~init:String.Set.empty t.calls |>
    Set.to_list

  let successed = fold_calls ~condition:(fun data -> fst data <> 0)
  let abs_successed = fold_calls ~condition:(fun data -> snd data = 0)
  let misexecuted = fold_calls ~condition:(fun data -> snd data <> 0)
  let abs_misexecuted = fold_calls ~condition:(fun data -> fst data = 0)

  let mislifted t =
    List.fold_left ~init:String.Set.empty
      ~f:(fun names errs ->
          match errs with
          | `Lifter_error (insn,_) -> Set.add names insn
          | _ -> names) t.errors |>
    Set.to_list
end

let print_table :
  Format.formatter -> (string * ('a -> string)) list -> 'a list -> unit =
  fun fmt info rows ->
  let open Veri_tab in
  let headers, processors  =
    List.map ~f:fst info, List.map ~f:snd info in
  List.foldi rows ~init:(create headers)
    ~f:(fun i tab row ->
        let cells =
          List.fold ~init:[] processors ~f:(fun acc p -> p row :: acc) in
        add_row tab @@ List.rev cells) |>
  pp fmt

module Summary = struct

  type nonrec t = t [@@deriving bin_io, compare, sexp]

  type p = {
    name: string;
    rel : float;
    abs : int;
  } [@@deriving bin_io, sexp, compare]

  let of_stat s =
    let make name abs rel = {name; abs; rel;} in
    if Abs.total s = 0 then []
    else
      let as_percents = true in
      [ make "overloaded"  (Abs.overloaded s)  (Rel.overloaded ~as_percents s);
        make "undisasmed"  (Abs.undisasmed s)  (Rel.undisasmed ~as_percents s);
        make "misexecuted" (Abs.misexecuted s) (Rel.misexecuted ~as_percents s);
        make "mislifted"   (Abs.mislifted s)   (Rel.mislifted ~as_percents s);
        make "damaged"     (Abs.damaged s)     (Rel.damaged ~as_percents s);
        make "successed"   (Abs.successed s)   (Rel.successed ~as_percents s);]

  let pp fmt t = match of_stat t with
    | [] -> Format.fprintf fmt "summary is unavailable\n"
    | ps ->
      print_table fmt
        ["", (fun x -> x.name);
         "rel", (fun x -> Printf.sprintf "%.2f%%" x.rel);
         "abs",  (fun x -> Printf.sprintf "%d" x.abs);]
        ps

end

let merge ts =
  let (+) s s' =
    let errors = s.errors @ s'.errors in
    let calls = Map.fold ~init:s.calls s'.calls
        ~f:(fun ~key ~data calls ->
            Map.change calls key ~f:(function
                | None -> Some data
                | Some (ok,er) -> Some (fst data + ok, snd data + er))) in
    {errors; calls} in
  List.fold ~f:(+) ~init:empty ts

let pp_summary = Summary.pp

include Regular.Make(struct
    type nonrec t = t [@@deriving bin_io, compare, sexp]
    let compare = compare
    let hash = Hashtbl.hash
    let module_name = Some "Veri_stat"
    let version = "0.1"

    let pp_misexecuted fmt = function
      | [] -> ()
      | mis ->
        Format.fprintf fmt "misexecuted \n";
        print_table fmt
          [ "instruction", fst;
            "failed", (fun (_, (_,er)) -> Printf.sprintf "%d" er);
            "successful", (fun (_, (ok,_)) -> Printf.sprintf "%d" ok); ]
          mis

    let pp_mislifted fmt names =
      let max_row_len = 10 in
      let max_col_cnt = 5 in
      match names with
      | [] -> ()
      | names when List.length names <= max_row_len ->
        let names' = "mislifted:" :: names in
        List.iter ~f:(Format.fprintf fmt "%s ") names';
        Format.print_newline ()
      | names ->
        let rows, row, _ = List.fold ~init:([], [], 0)
            ~f:(fun (acc, row, i) name ->
                if i < max_col_cnt then acc, name :: row, i + 1
                else row :: acc, name :: [], 1) names in
        let gaps = Array.create ~len:(max_col_cnt - List.length row) "-----" in
        let last = row @ Array.to_list gaps in
        let rows = List.rev (last :: rows) in
        let make_col i = "mislifted", (fun row -> List.nth_exn row i) in
        let cols = [
          make_col 0; make_col 1; make_col 2; make_col 3; make_col 4; ] in
        print_table fmt cols rows

    let pp fmt t =
      let misexec =
        List.filter ~f:(fun (_,(_,er)) -> er <> 0) (Map.to_alist t.calls) in
      let mislift = Names.mislifted t in
      Format.fprintf fmt "%a\n%a\n"
        pp_misexecuted misexec pp_mislifted mislift

  end)
