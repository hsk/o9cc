let _ =
  if Array.length (Sys.argv) != 2 then (
      Printf.printf("Usage o9cc n\n");
      exit(1)
  ) else
  let file = open_out "tmp.s" in
  Printf.fprintf file ".intel_syntax noprefix\n";
  Printf.fprintf file ".globl _main\n";
  Printf.fprintf file "_main:\n";
  Printf.fprintf file "  mov rax, %s\n" (Sys.argv.(1));
  Printf.fprintf file "  ret\n";
  ()
