
open Ast
let rec gen ast file =
  match ast.value with
  | Num(n) ->
    Printf.fprintf file "  push %d\n" n
  | BinOp(op, l, r) ->
    gen l file;
    gen r file;
    Printf.fprintf file "  pop rdi\n";
    Printf.fprintf file "  pop rax\n";
    begin match op.value with
    | Add  -> Printf.fprintf file "  add rax, rdi\n"
    | Sub  -> Printf.fprintf file "  sub rax, rdi\n"
    | Mult -> Printf.fprintf file "  imul rax, rdi\n"
    | Div  -> Printf.fprintf file "  cqo\n";
              Printf.fprintf file "  idiv rdi\n";
    | Eq   -> Printf.fprintf file "  cmp rax, rdi\n";
              Printf.fprintf file "  sete al\n";
              Printf.fprintf file "  movzx rax, al\n"
    | Ne   -> Printf.fprintf file "  cmp rax, rdi\n";
              Printf.fprintf file "  setne al\n";
              Printf.fprintf file "  movzx rax, al\n"
    | Lt   -> Printf.fprintf file "  cmp rax, rdi\n";
              Printf.fprintf file "  setl al\n";
              Printf.fprintf file "  movzx rax, al\n";
    | Le   -> Printf.fprintf file "  cmp rax, rdi\n";
              Printf.fprintf file "  setle al\n";
              Printf.fprintf file "  movzx rax, al\n"
    end;
    Printf.fprintf file "  push rax\n"
