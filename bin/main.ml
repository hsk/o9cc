open O9cc
let _ =
  let file = open_out "tmp.s" in
  let ast = Parser.translation_unit Lexer.token (Lexing.from_channel stdin) in
  Ast.write_dot ast "tmp.dot";

  Printf.fprintf file ".intel_syntax noprefix\n";
  Printf.fprintf file ".globl _main\n";
  Printf.fprintf file "_main:\n";
  Generator.gen ast file;
  Printf.fprintf file "  pop rax\n";
  Printf.fprintf file "  ret\n";
  ()
