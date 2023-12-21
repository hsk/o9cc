type t = OPERATOR of string | INTEGER of int | EOF

let tokenize f lexbuf =
  let rec loop ls =
    match f lexbuf with
    | EOF -> List.rev ls
    | e -> loop (e::ls)
  in
  loop []
