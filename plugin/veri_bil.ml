open Core_kernel
open Bap.Std
open Bap_traces.Std
open Bap_future.Std
open Veri_policy
open Monads.Std

include Self()
open Bap_main

module Dis = Disasm_expert.Basic

let read_rules fname =
  let comments = "#" in
  let is_sensible s =
    String.(s <> "") && not (String.is_prefix ~prefix:comments s) in
  let inc = In_channel.create fname in
  let strs = In_channel.input_lines inc in
  In_channel.close inc;
  List.map ~f:String.strip strs
  |> List.filter ~f:is_sensible
  |> List.map ~f:Veri_rule.of_string_err |>
  List.filter_map ~f:(function
      | Ok r -> Some r
      | Error er ->
        Format.(fprintf std_formatter "%s\n" (Error.to_string_hum er));
        None)

let string_of_error = function
  | `Protocol_error er ->
    Printf.sprintf "protocol error: %s"
      (Info.to_string_hum (Error.to_info er))
  | `System_error er ->
    Printf.sprintf "system error: %s" (Caml_unix.error_message er)
  | `No_provider -> "no provider"
  | `Ambiguous_uri -> "ambiguous uri"

let default_policy =
  let open Veri_rule in
  let p = Veri_policy.(add empty (create_exn ~insn:".*" ~left:".*" deny)) in
  Veri_policy.add p (create_exn ~insn:".*" ~right:".*" deny)

let make_policy = function
  | None -> default_policy
  | Some file ->
    read_rules file |>
    List.fold ~f:Veri_policy.add ~init:Veri_policy.empty

let errors_stream s =
  let pp_result fmt report  =
    Format.fprintf fmt "%a" Veri_report.pp report;
    Format.print_flush () in
  ignore(Stream.subscribe s (pp_result Format.std_formatter) : Stream.id)

let eval_file file policy show_errs =
  let mk_er s = Error (Error.of_string s) in
  let uri = Uri.of_string ("file:" ^ file) in
  match Trace.load uri with
  | Error er ->
    Printf.sprintf "error during loading trace: %s\n" (string_of_error er) |>
    mk_er
  | Ok trace ->
    match Dict.find (Trace.meta trace) Meta.arch with
    | None -> mk_er "trace of unknown arch"
    | Some arch ->
      Dis.with_disasm ~backend:"llvm" (Arch.to_string arch) ~f:(fun dis ->
          let dis = Dis.store_asm dis |> Dis.store_kinds in
          let stat = Veri_stat.empty in
          let ctxt = new Veri.context stat policy trace in
          let veri = new Veri.t arch dis in
          if show_errs then errors_stream ctxt#reports;
          let ctxt' = Monad.State.exec (veri#eval_trace trace) ctxt in
          Ok ctxt'#stat)

let read_dir path =
  let dir = Caml_unix.opendir path in
  let fullpath file = String.concat ~sep:"/" [path; file] in
  let next () =
    try
      Some (Caml_unix.readdir dir)
    with End_of_file -> None in
  let rec folddir acc =
    match next () with
    | Some file -> folddir (fullpath file :: acc)
    | None -> acc in
  let files = folddir [] in
  Caml_unix.closedir dir;
  files

let main path rules out show_errs show_stat _ctxt =
  let files =
    if Sys.is_directory path then (read_dir path)
    else [path] in
  let policy = make_policy rules in
  let eval stats file =
    Format.(fprintf std_formatter "%s@." file);
    match eval_file file policy show_errs with
    | Error er ->
      Error.to_string_hum er |>
      Printf.eprintf "error in verification: %s";
      stats
    | Ok stat' -> (Filename.basename file, stat') :: stats in
  let stats = List.fold ~init:[] ~f:eval files in
  let stat = Veri_stat.merge (List.map ~f:snd stats) in
  if show_stat then
    Veri_stat.pp Format.std_formatter stat;
  Format.(fprintf std_formatter "%a\n" Veri_stat.pp_summary stat);
  match out with
  | None -> Ok ()
  | Some out -> Ok (Veri_out.output stats out)

let input =
  Extension.Command.argument
    ~doc:"Input trace file or directory with trace files"
    Extension.Type.("FILE | DIR" %: path)

let output =
  Extension.Command.parameter ~doc:"File to output results"
    Extension.Type.("FILE"  %: some path)
    "output"

let rules =
  Extension.Command.parameter ~doc:"File with policy description"
    Extension.Type.("FILE" %: some non_dir_file)
    "rules"

let show_errors =
  Extension.Command.flag
    ~doc:"Show detailed information about BIL errors"
    "show-errors"

let show_stat =
  Extension.Command.flag
    ~doc:"Show verification statistics"
    "show-stat"

let man =
  {|Bil verification"
   Veri is a BIL verification tool and intend to verify BAP lifters
   and to find errors."; |}

let features_used = [
  "disassembler";
  "lifter";
]

let _ = (Extension.Command.(begin
      declare ~doc:man "veri"
        ~requires:features_used
        (args $input $rules $output $show_errors $show_stat)
    end) @@ main : unit)
