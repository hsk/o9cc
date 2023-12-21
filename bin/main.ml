open O9cc
let _ =
  let frame,program = Parser.translation_unit Lexer.token (Lexing.from_channel stdin) in
  Ast.write_dot program "tmp.dot";
  let file = open_out "tmp.s" in
  Generator.codegen program frame file
