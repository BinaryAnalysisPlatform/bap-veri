open Core_kernel[@@warning "-D"]

type t = [
  | `Overloaded_chunk         (** chunk contains more then one instruction  *)
  | `Damaged_chunk of Error.t (** chunk data can't be transformed to memory *)
  | `Disasm_error  of Error.t (** chunk data can't be disasmed        *)
  | `Lifter_error  of string * Error.t (** chunk data can't be lifted *)
] [@@deriving bin_io, compare, sexp]
