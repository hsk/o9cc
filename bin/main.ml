open O9cc
let _ =
  if Array.length (Sys.argv) != 2 then (
      Printf.printf("Usage o9cc n\n");
      exit(1)
  ) else
  let file = open_out "tmp.s" in
  let first_val,tokens = match Parser.tokenize Lexer.token (Lexing.from_string (Sys.argv.(1))) with
    | INTEGER first_val::tokens -> first_val,tokens
    | _ -> Printf.eprintf "Tokenize error"; exit 1
  in
  Printf.fprintf file ".intel_syntax noprefix\n";
  Printf.fprintf file ".globl _main\n";
  Printf.fprintf file "_main:\n";
  Printf.fprintf file "  mov rax, %d\n" first_val;
  tokens|>List.iter(function
    | Parser.INTEGER(i) -> Printf.fprintf file "%d\n" i
    | Parser.OPERATOR("+") -> Printf.fprintf file "  add rax, "
    | Parser.OPERATOR("-") -> Printf.fprintf file "  sub rax, "
    | _ -> Printf.eprintf "Unknown operator"; exit 1
  );
  Printf.fprintf file "  ret\n";
  ()
