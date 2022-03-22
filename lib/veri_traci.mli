open Bap.Std
open Bap_traces.Std

type 'a u = 'a Bil.Result.u [@@warning "-D"]
type event = Trace.event

class context : Trace.t -> object('s)
    inherit Bili.context [@@warning "-D"]
    method next_event: ('s * event) option
    method with_events: Trace.t -> 's
  end


class ['a] t: arch -> object('s)
  constraint 'a = #context
  inherit ['a] Bili.t [@@warning "-D"]
  method eval_trace : Trace.t -> 'a u
  method eval_event : event -> 'a u
  method eval_memory_load  : addr move -> 'a u
  method eval_memory_store : addr move -> 'a u
  method eval_register_read : var move -> 'a u
  method eval_register_write : var move -> 'a u
  method eval_exec : chunk -> 'a u
  method eval_pc_update : addr -> 'a u
  method eval_context_switch : int -> 'a u
  method eval_mode: Mode.t -> 'a u
  method eval_syscall : syscall -> 'a u
  method eval_exn : exn -> 'a u
  method eval_call : call -> 'a u
  method eval_return : return -> 'a u
  method eval_modload : modload -> 'a u
end
