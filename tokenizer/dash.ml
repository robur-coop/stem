(* TODO(dinosaure): handle [encoding]. *)
let find_matches ?encoding:_ str =
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

let find_matches_on_bstr ?encoding:_ bstr =
  match Bstr.split_on_char '-' bstr with
  | [] -> assert false
  | [ bstr ] ->
      let str = Bstr.to_string bstr in
      List.to_seq [ { S.str; is_match = false } ]
  | hd :: tl ->
      let dash = { S.str = "-"; is_match = true } in
      let rec go acc = function
        | [] -> List.to_seq (List.rev acc)
        | bstr :: r when Bstr.is_empty bstr -> go (dash :: acc) r
        | bstr :: r ->
            let str = Bstr.to_string bstr in
            let x = { S.str; is_match = false } in
            go (x :: dash :: acc) r in
      let hd = Bstr.to_string hd in
      go [ { S.str = hd; is_match = false } ] tl
