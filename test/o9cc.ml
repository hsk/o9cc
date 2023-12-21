open O9cc
let _ = Printf.printf "test start\n"

let tokenize f lexbuf =
  let rec loop ls =
    match f lexbuf with
    | Parser.EOF -> List.rev ls
    | e -> loop (e::ls)
  in
  loop []

(* test *)
(* 5 + 20-4 *)
let () = (* test_f0 *)
  let tokens = tokenize Lexer.token (Lexing.from_string "5+20") in
  assert(tokens=[Parser.INTEGER(5);Parser.PLUS;Parser.INTEGER(20)])
let () = (*test_f1 *)
  let tokens = tokenize Lexer.token (Lexing.from_string "5 + 20-4") in
  assert(tokens=[Parser.INTEGER(5);Parser.PLUS;Parser.INTEGER(20);Parser.MINUS;Parser.INTEGER(4)])

let _ = Printf.printf "test end\n"
