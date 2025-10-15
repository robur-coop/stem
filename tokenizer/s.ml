type token = { str : string; is_match : bool }

module type PATTERN = sig
  val find_matches : ?encoding:Snowball.encoding -> string -> token Seq.t

  val find_matches_on_bstr :
    ?encoding:Snowball.encoding -> Bstr.t -> token Seq.t
end

type pattern = (module PATTERN)
