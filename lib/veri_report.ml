open Core_kernel
open Bap.Std
open Bap_traces.Std
open Regular.Std

type event = Trace.event [@@deriving bin_io, compare, sexp]
type rule = Veri_rule.t [@@deriving bin_io, compare, sexp]
type matched = Veri_policy.matched [@@deriving bin_io, compare, sexp]

type t = {
  bil  : bil;
  insn : string;
  code : string;
  mode : Mode.t option;
  left : event list;
  right: event list;
  data : (rule * matched) list;
} [@@deriving bin_io, compare, fields, sexp]

let create = Fields.create

include Regular.Make(struct
    type nonrec t = t [@@deriving bin_io, compare, sexp]

    let compare = compare
    let hash = Hashtbl.hash
    let module_name = Some "Veri.Report"
    let version = "0.1"

    let pp_code fmt s =
      let pp fmt s =
        String.iter ~f:(fun c -> Format.fprintf fmt "%02X " (Char.to_int c)) s in
      Format.fprintf fmt "@[<h>%a@]" pp s

    let pp_mode fmt = function
      | Some m -> Format.fprintf fmt "(%a)" Mode.pp m
      | None -> ()

    let pp_evs fmt evs =
      List.iter ~f:(fun ev ->
          Format.(fprintf std_formatter "%a; " Value.pp ev)) evs

    let pp_data fmt (rule, matched) =
      let open Veri_policy in
      Format.fprintf fmt "%a\n%a" Veri_rule.pp rule Matched.pp matched

    let pp fmt t =
      let bil = Stmt.simpl t.bil in
      Format.fprintf fmt "@[<v>%s %a%a@,left:  %a@,right: %a@,%a@]@."
        t.insn pp_code t.code pp_mode t.mode pp_evs t.left pp_evs t.right Bil.pp bil;
      List.iter ~f:(pp_data fmt) t.data;
      Format.print_newline ()

  end)
