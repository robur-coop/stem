let files =
  [
    "arabic.txt";
    "danish.txt";
    "dutch.txt";
    "english.txt";
    "finnish.txt";
    "french.txt";
    "german.txt";
    "hungarian.txt";
    "indonesian.txt";
    "italian.txt";
    "norwegian.txt";
    "portuguese.txt";
    "russian.txt";
    "spanish.txt";
    "swedish.txt";
  ]

module S = Set.Make (String)

let dictionary_from_file filename =
  let ic = open_in filename in
  let finally () = close_in ic in
  Fun.protect ~finally @@ fun () ->
  let rec go m =
    match input_line ic with
    | exception End_of_file -> m
    | line ->
        let[@warning "-8"] (str :: _) = String.split_on_char '|' line in
        let str = String.trim str in
        if String.length str > 0 then go (S.add str m) else go m in
  go S.empty

(* Copyright (c) 2014 The fmt programmers. All rights reserver.
   SPDX-License-Identifier: ISC *)

let sp ppf _ = Format.pp_print_space ppf ()
let cut ppf _ = Format.pp_print_cut ppf ()
let fmt fmt ppf = Format.fprintf ppf fmt

let semi ppf _ =
  Format.pp_print_string ppf ";" ;
  sp ppf ()

let box ?(indent = 0) pp_elt ppf elt =
  let open Format in
  pp_open_box ppf indent ;
  pp_elt ppf elt ;
  pp_close_box ppf ()

let surround str0 str1 pp_elt ppf elt =
  let open Format in
  pp_print_string ppf str0 ;
  pp_elt ppf elt ;
  pp_print_string ppf str1

let iter ?sep:(pp_sep = cut) iter pp_elt ppf =
  let is_first = ref true in
  let pp_elt elt =
    if !is_first then is_first := false else pp_sep ppf () ;
    pp_elt ppf elt in
  iter pp_elt

let list ?sep pp_elt = iter ?sep List.iter pp_elt
let brackets pp_elt = box ~indent:1 (surround "[" "]" pp_elt)
let pp_list pp_elt = brackets (list ~sep:semi (box pp_elt))

let dictionary_to_ocaml_module m filename =
  let oc = open_out filename in
  let finally () = close_out oc in
  Fun.protect ~finally @@ fun () ->
  let ppf = Format.formatter_of_out_channel oc in
  let lst = S.to_list m in
  Format.fprintf ppf "let words = @[<hov>%a@] ;;" (pp_list (fmt "%S")) lst

let () =
  let fn filename =
    let basename = Filename.chop_extension filename in
    let m = dictionary_from_file filename in
    dictionary_to_ocaml_module m (basename ^ ".ml") in
  List.iter fn files
