open Core_kernel[@@warning "-D"]
open Text_block

module Abs = Veri_stat.Abs
module Rel = Veri_stat.Rel

let make_iota max =
  let rec make acc n =
    if n < 0 then acc
    else make (n :: acc) (n - 1) in
  make [] (max - 1)

let make_col title to_string vals =
  vcat ~align:`Center (text title :: List.map ~f:(fun x -> text (to_string x)) vals)

let texts_col title vals = make_col title Fn.id vals
let intgr_col title vals = make_col title (Printf.sprintf "%d") vals
let float_col title vals = make_col title (Printf.sprintf "%.2f") vals

let output stats path =
  let of_stats f = List.map ~f stats in
  let of_stats' f = List.map ~f:(fun x -> f (snd x)) stats in
  let out = Out_channel.create path in
  let cnter = intgr_col "#" (make_iota (List.length stats)) in
  let names = texts_col "file" (of_stats fst) in
  let total = intgr_col "total" (of_stats' Abs.total) in
  let as_percents = true in
  let prcnt = List.map
      ~f:(fun (name, f) -> float_col name (of_stats' f))
      [ "successed, %",   Rel.successed ~as_percents;
        "misexecuted, %", Rel.misexecuted ~as_percents;
        "overloaded, %",  Rel.overloaded ~as_percents;
        "damaged, %",     Rel.damaged ~as_percents;
        "undisasmed, %",  Rel.undisasmed ~as_percents;
        "mislifted, %",   Rel.mislifted ~as_percents; ] in
  let tab = hcat ~sep:(text "  |  ") ([cnter; names; total] @ prcnt) in
  Out_channel.output_string out (render tab);
  Out_channel.close out
