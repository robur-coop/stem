let re = Re.Pcre.re "\\w+|[^\\w\\s]+"
let re = Re.compile re

let find_matches ?off:pos ?len str =
  let fn = function
    | `Delim g ->
        let str = Re.Group.get g 0 in
        { S.str; is_match = false }
    | `Text str -> { S.str; is_match = true } in
  Re.Seq.split_full ?pos ?len re str |> Seq.map fn
