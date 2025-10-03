type behavior = Remove | Isolate | Merge_with_previous | Merge_with_next

let split ~pattern:(module Pattern : S.PATTERN) ?(behavior = Remove) str =
  let matches = Pattern.find_matches str in
  match behavior with
  | Remove ->
      let fn = function
        | { S.is_match = true; _ } -> None
        | { str; _ } -> Some str in
      Seq.filter_map fn matches
  | Isolate ->
      let fn { S.str; _ } = str in
      Seq.map fn matches
  | Merge_with_previous ->
      let fn (previous_match, acc) elt =
        match (previous_match, acc) with
        | _, [] -> (Some elt, [ elt.S.str ])
        | None, acc -> (Some elt, elt.str :: acc)
        | Some { S.is_match; _ }, prev :: acc ->
            if is_match = false && elt.is_match = true
            then (None, (prev ^ elt.str) :: acc)
            else (Some elt, elt.str :: prev :: acc) in
      let _, sstr = Seq.fold_left fn (None, []) matches in
      List.(to_seq (List.rev sstr))
  | Merge_with_next ->
      let fn (previous_match, acc) elt =
        match (previous_match, acc) with
        | _, [] -> (Some elt, [ elt.S.str ])
        | None, acc -> (Some elt, elt.str :: acc)
        | Some { S.is_match; _ }, prev :: acc ->
            if is_match = true && elt.is_match = false
            then (None, (prev ^ elt.str) :: acc)
            else (Some elt, elt.str :: prev :: acc) in
      let _, sstr = Seq.fold_left fn (None, []) matches in
      List.(to_seq (List.rev sstr))

let whitespace doc = split ~pattern:(module Whitespace) ~behavior:Remove doc
