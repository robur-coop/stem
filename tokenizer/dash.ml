let find_matches ?(off = 0) ?len str =
  let len = match len with None -> String.length str - off | Some len -> len in
  let str = String.sub str off len in
  match String.split_on_char '-' str with
  | [] -> assert false
  | [ str ] -> List.to_seq [ { S.str; is_match = false } ]
  | hd :: tl ->
      let dash = { S.str = "-"; is_match = true } in
      let rec go acc = function
        | [] -> List.to_seq (List.rev acc)
        | "" :: r -> go (dash :: acc) r
        | str :: r ->
            let x = { S.str; is_match = false } in
            go (x :: dash :: acc) r in
      go [ { S.str = hd; is_match = false } ] tl
