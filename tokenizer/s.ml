type token = { str : string; is_match : bool }

module type PATTERN = sig
  val find_matches : ?encoding:Snowball.encoding -> string -> token Seq.t
end

type pattern = (module PATTERN)
