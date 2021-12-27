open Core_kernel
open Regular.Std

type trial = Pcre.regexp

let empty = ""
let trial_exn s = Pcre.regexp ~flags:[`ANCHORED] s
let er s = Error (Error.of_string s)

module Action = struct
  type t = Skip | Deny [@@deriving bin_io, compare, sexp]
  let skip = Skip
  let deny = Deny

  let to_string = function
    | Skip -> "SKIP"
    | Deny -> "DENY"

  let of_string_err = function
    | "SKIP" -> Ok Skip
    | "DENY" -> Ok Deny
    | s -> er (Printf.sprintf "only SKIP | DENY actions should be used: %s" s)
end

module Field = struct
  type t = trial * string

  let create_exn s = trial_exn s, s

  let create_err s =
    try
      Ok (create_exn s)
    with Pcre.Error _ ->
      let s = Printf.sprintf "error in field %s" s in
      Error (Error.of_string s)

  let is_empty f = String.equal (snd f) empty
end

type action = Action.t [@@deriving bin_io, compare, sexp]
type field = Field.t

type t = {
  action : action;
  insn   : field;
  both   : field;
  left   : field;
  right  : field;
} [@@deriving fields]

exception Bad_field of string

let skip = Action.skip
let deny = Action.deny
let is_empty = Field.is_empty

let contains_backreference =
  let rex = Pcre.regexp "\\\\[1-9]" in
  fun s -> Pcre.pmatch ~rex s

let right_field s =
  if contains_backreference s then
    Ok (trial_exn empty, s)
  else Field.create_err s

let create ?insn ?left ?right action =
  let open Result in
  let of_opt = Option.value_map ~default:empty ~f:ident in
  let both = String.concat ~sep:" " [of_opt left; of_opt right] in
  Field.create_err (of_opt insn) >>= fun insn ->
  Field.create_err both >>= fun both ->
  Field.create_err (of_opt left) >>= fun left ->
  right_field (of_opt right) >>= fun right ->
  Ok ({action; insn; both; left; right;})

let create_exn ?insn ?left ?right action =
  match create ?insn ?left ?right action with
  | Ok r -> r
  | Error s -> raise (Bad_field (Error.to_string_hum s))

let match_field t field s =
  let field' = match field with
    | `Insn -> t.insn
    | `Both -> t.both
    | `Left -> t.left
    | `Right -> t.right in
  Pcre.pmatch ~rex:(fst field') s

module S = struct

  type nonrec t = t

  let to_string t =
    let contains_space s = String.exists ~f:(Char.equal ' ') s in
    let of_field f =
      if Field.is_empty f then "''"
      else if contains_space (snd f) then Printf.sprintf "'%s'" (snd f)
      else snd f in
    Printf.sprintf "%s %s %s %s" (Action.to_string t.action)
      (of_field t.insn) (of_field t.left) (of_field t.right)

  let rex = Pcre.regexp "'.*?'|\".*?\"|\\S+"
  let is_quote c = Char.equal c '\"' || Char.equal c '\''
  let unquote s = String.strip ~drop:is_quote s

  (** Not_found could be raised here  *)
  let fields_exn str =
    Pcre.exec_all ~rex str |>
    Array.fold ~init:[] ~f:(fun acc ar ->
        let subs = Pcre.get_substrings ar in
        acc @ Array.to_list subs) |>
    List.map ~f:unquote

  let fields_opt str =
    try
      match fields_exn str with
      | [action; insn; left; right] -> Some (action, insn, left, right)
      | _ -> None
    with _ -> None

  let fields_err str =
    match fields_opt str with
    | Some fields -> Ok fields
    | None ->
      er (Printf.sprintf "String %s doesn't match to rule grammar" str)

  let of_string_err s =
    let open Or_error in
    fields_err s >>= fun (action, insn, left, right) ->
    Action.of_string_err action >>= fun action' ->
    create ~insn ~left ~right action'

  let of_string str = ok_exn (of_string_err str)

end

let of_string_err = S.of_string_err

include Sexpable.Of_stringable(S)
include Binable.Of_stringable(S)
include (S : Stringable with type t := t)

include Regular.Make(struct
    type nonrec t = t [@@deriving bin_io, compare, sexp]
    let compare = compare
    let hash = Hashtbl.hash
    let module_name = Some "Veri_rule"
    let version = "0.1"

    let pp fmt t = Format.fprintf fmt "%s" (to_string t)
  end)
