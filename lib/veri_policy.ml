open Core_kernel
open Graph
open Bap.Std
open Bap_traces.Std
open Regular.Std

module Rule = Veri_rule

type event = Trace.event [@@deriving bin_io, compare, sexp]
type events = Value.Set.t
type rule = Rule.t [@@deriving bin_io, compare, sexp]

module Matched = struct
  type t = event list * event list [@@deriving bin_io, compare, sexp]

  include Regular.Make(struct
      type nonrec t = t [@@deriving bin_io, compare, sexp]
      let compare = compare
      let hash = Hashtbl.hash
      let module_name = Some "Veri_policy.Matched"
      let version = "0.1"
      let pp_ev fmt ev = Format.fprintf fmt "%a; " Value.pp ev

      let pp_events pref fmt = function
        | [] -> ()
        | evs ->
          Format.fprintf fmt "%s: " pref;
          List.iter evs ~f:(pp_ev fmt);
          Format.print_newline ()

      let pp fmt (evs, evs') =
        Format.fprintf fmt "%a%a"
          (pp_events "left") evs (pp_events "right") evs'

    end)
end

type matched = Matched.t [@@deriving bin_io, compare, sexp]
type t = rule list [@@deriving bin_io, compare, sexp]

let empty = []
let add t rule : t = rule :: t

let string_of_events ev ev' =
  String.concat ~sep:" " [Value.pps () ev; Value.pps () ev']

let sat_events r ev ev' =
  Value.Typeid.equal (Value.typeid ev) (Value.typeid ev') &&
  Rule.match_field r `Both (string_of_events ev ev')

module G = struct
  type t = rule * event array * event array
  module V = struct
    type t = Source | Sink | Person of int | Task of int [@@deriving compare]
    let hash = Hashtbl.hash
    let equal x y = compare x y = 0
  end
  module E = struct
    type label = unit

    type t = {src : V.t; dst : V.t} [@@deriving fields]
    let make src dst = {src; dst}
    let label _ = ()
  end
  type dir = Succ | Pred

  let iter dir f (rule, workers, jobs) v =
    match v,dir with
    | V.Source,Pred -> ()
    | V.Source,Succ ->
      Array.iteri workers ~f:(fun i _  ->
          f @@ E.make V.Source (V.Person i))
    | V.Sink,Pred ->
      Array.iteri jobs ~f:(fun i _ ->
          f @@ E.make (V.Task i) V.Sink)
    | V.Sink,Succ -> ()
    | V.Person i as p,Pred ->
      f @@ E.make V.Source p
    | V.Person i as p,Succ ->
      Array.iteri jobs ~f:(fun j job ->
          if sat_events rule workers.(i) job
          then f (E.make p (V.Task j)))
    | V.Task j as t,Succ ->
      f @@ E.make t V.Sink
    | V.Task j as t,Pred ->
      Array.iteri workers ~f:(fun i worker ->
          if sat_events rule worker jobs.(j)
          then f (E.make (V.Person i) t))

  let iter_succ_e = iter Succ
  let iter_pred_e = iter Pred
end

module F = struct
  type t = int
  type label = unit
  let max_capacity () = 1
  let min_capacity () = 0
  let flow () = min_capacity ()
  let add = (+)
  let sub = (-)
  let zero = 0
  let compare = Int.compare
end

module FFMF = Flow.Ford_Fulkerson(G)(F)

let single_match fmatch events =
  let f ev = fmatch (Value.pps () ev) in
  List.filter ~f (Set.to_list events)

let match_right rule events =
  match single_match (Rule.match_field rule `Right) events with
  | [] -> None
  | ms -> Some ([], ms)

let match_left rule events =
  match single_match (Rule.match_field rule `Left) events with
  | [] -> None
  | ms -> Some (ms,[])

let match_both rule left right =
  let workers = Set.to_array left in
  let jobs = Set.to_array right in
  let (flow,_) = FFMF.maxflow (rule, workers, jobs) G.V.Source G.V.Sink in
  Array.foldi workers ~init:([],[])
    ~f:(fun i (acc, acc') w ->
        match Array.findi jobs ~f:(fun j e ->
            flow (G.E.make (G.V.Person i) (G.V.Task j)) <> 0) with
        | None -> acc, acc'
        | Some (_,e) -> w :: acc, e :: acc') |> function
  | [], [] -> None
  | ms -> Some ms

let diff events events' =
  let left = Set.diff events events' in
  let right = Set.diff events' events in
  let to_str e = Value.pps () e in
  let is_unique other ev =
    not (Set.exists other ~f:(fun ev' -> String.equal (to_str ev) (to_str ev'))) in
  Set.filter ~f:(is_unique right) left,
  Set.filter ~f:(is_unique left) right

let match_events rule insn events events' =
  match Rule.match_field rule `Insn insn with
  | false -> None
  | true ->
    let left, right = diff events events' in
    match Rule.(is_empty (left rule)), Rule.(is_empty (right rule)) with
    | true, true -> Some (Set.to_list events, Set.to_list events')
    | true, _ -> match_right rule right
    | _, true -> match_left rule left
    | _ -> match_both rule left right

let remove what from =
  let eq x y = Value.compare x y = 0 in
  let not_exists e = not (List.exists what ~f:(fun e' -> eq e e')) in
  Set.filter ~f:not_exists from

let remove_matched events events' (ms, ms') =
  remove ms events, remove ms' events'

let denied rules insn events events' =
  let rec loop acc rules (evs,evs') = match rules with
    | [] -> acc
    | rule :: rls ->
      match match_events rule insn evs evs' with
      | None -> loop acc rls (evs,evs')
      | Some matched ->
        let acc' =
          if Veri_rule.compare_action (Rule.action rule) Rule.skip = 0 then acc
          else (rule, matched) :: acc in
        remove_matched evs evs' matched |>
        loop acc' rls in
  loop [] (List.rev rules) (events, events')
