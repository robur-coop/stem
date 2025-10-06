[@@@warning "-32"]

type encoding = ISO_8859_1 | ISO_8859_2 | KOI8_R | UTF_8

let encoding_to_string = function
  | ISO_8859_1 -> "ISO_8859_1"
  | ISO_8859_2 -> "ISO_8859_2"
  | KOI8_R -> "KOI8_R"
  | UTF_8 -> "UTF_8"

let pp_encoding ppf v = Format.pp_print_string ppf (encoding_to_string v)

type t

exception Stem_internal_error of string

let _ = Callback.register_exception "stem exception" (Stem_internal_error "")

module C = struct
  external list : unit -> string list = "stem_list"
  external stem_new : string -> string -> t = "stem_new"
  external stem_delete : t -> unit = "stem_delete"
  external stem : t -> string -> string = "stem"
end

module Language = struct
  type t = string

  let pp = Format.pp_print_string
end

let languages = C.list ()
let porter = List.find (( = ) "porter") languages

let create ?(encoding = UTF_8) alg =
  let encoding = encoding_to_string encoding in
  C.stem_new alg encoding

let remove = C.stem_delete
let stem t word = C.stem t word
