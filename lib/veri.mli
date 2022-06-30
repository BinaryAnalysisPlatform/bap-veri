open Core_kernel[@@warning "-D"]
open Bap.Std
open Regular.Std
open Bap_traces.Std
open Bap_future.Std

module Disasm : sig
  module Dis = Disasm_expert.Basic
  open Dis
  type t = (asm, kinds) Dis.t
end

class context: Veri_stat.t -> Veri_policy.t -> Trace.t -> object('s)
    inherit Veri_traci.context
    method split : 's
    method merge : 's
    method other  : 's option
    method save: 's -> 's
    method switch: 's
    method stat : Veri_stat.t
    method code : Chunk.t option
    method mode : Mode.t option
    method events : Value.Set.t
    method reports : Veri_report.t stream
    method register_event : Trace.event -> 's
    method notify_error: Veri_error.t -> 's
    method set_bil : bil -> 's
    method set_code : Chunk.t -> 's
    method set_mode : Mode.t -> 's
    method set_insn: string -> 's
    method drop_pc : 's
  end

class ['a] t : arch -> object('s)
    constraint 'a = #context
    inherit ['a] Veri_traci.t
  end
