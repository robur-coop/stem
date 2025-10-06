type encoding = ISO_8859_1 | ISO_8859_2 | KOI8_R | UTF_8

val pp_encoding : Format.formatter -> encoding -> unit

type t

module Language : sig
  type t = private string

  val pp : Format.formatter -> t -> unit
end

val languages : Language.t list
val porter : Language.t
val create : ?encoding:encoding -> Language.t -> t
val remove : t -> unit
val stem : t -> string -> string
