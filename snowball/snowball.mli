(** Type of encodings. *)
type encoding = ISO_8859_1 | ISO_8859_2 | KOI8_R | UTF_8

val pp_encoding : Format.formatter -> encoding -> unit

type t
(** Type of stemmers. *)

module Language : sig
  type t = private string
  (** Type of languages. *)

  val pp : Format.formatter -> t -> unit
end

val languages : Language.t list
(** Languages available for stemming. *)

val porter : Language.t

val create : ?encoding:encoding -> Language.t -> t
(** [create ?encoding language] creates a {i stemmer} which can be used to
    {i stem} words via {!val:stem}.

    {b NOTE}: it's important to {i release} (via {!val:remove} a {!type:t} when
    you are done about stemming. *)

val remove : t -> unit
(** [remove stemmer] destroys the underlying structure used to {i stem} words.
*)

val stem : t -> string -> string
(** [stem stemmer word] stems the given word with the given [stemmer] (which
    corresponds to the language we use). *)
