open Core_kernel[@@warning "-D"]
open Bap.Std
open Bap_traces.Std
open Regular.Std

type event = Trace.event
type events = Value.Set.t
type rule = Veri_rule.t

module Matched : sig
  type t = event list * event list [@@deriving bin_io, compare, sexp]
  include Regular.S with type t := t
end

type matched = Matched.t [@@deriving bin_io, compare, sexp]
type t [@@deriving bin_io, compare, sexp]

val empty : t
val add : t -> rule -> t

(** [match events rule insn left right] *)
val match_events: rule -> string -> events -> events -> matched option

(** [denied policy insn left right] *)
val denied: t -> string -> events -> events -> (rule * matched) list
