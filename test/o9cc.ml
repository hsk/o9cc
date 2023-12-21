open O9cc
let _ = Printf.printf "test start\n"
(* test *)
(* 5 + 20-4 *)
let () = (* test_f0 *)
  let tokens = Parser.tokenize Lexer.token (Lexing.from_string "5+20") in
  assert(tokens=[Parser.INTEGER(5);Parser.OPERATOR("+");Parser.INTEGER(20)])
let () = (*test_f1 *)
  let tokens = Parser.tokenize Lexer.token (Lexing.from_string "5 + 20-4") in
  assert(tokens=[Parser.INTEGER(5);Parser.OPERATOR("+");Parser.INTEGER(20);Parser.OPERATOR("-");Parser.INTEGER(4)])

let _ = Printf.printf "test end\n"
