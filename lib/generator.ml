
open Ast

module Depth_cnt = struct
  let add d depth =
    depth := !depth + d
  let sub d depth =
    depth := !depth - d
end

let gen_addr node output =
    match node.value with
    | LocalVar(s,_) ->
      let c = int_of_char (String.get s 0) in
      let offset = ((c - int_of_char 'a') + 1) * 8 in
      Printf.fprintf output "  lea %d(%%rbp), %%rax\n" (-offset)
    | _ ->
      failwith (Printf.sprintf "GEN: not an lvalue %s." (Ast.show_ast node))

let push output dc =
    Printf.fprintf output "  push %%rax\n";
    Depth_cnt.add 1 dc

let pop s output dc =
    Printf.fprintf output "  pop %s\n" s;
    Depth_cnt.sub 1 dc

let rec gen_expr node output dc =
  match node.value with
  | Num(n) ->
    Printf.fprintf output "  mov $%d, %%rax\n" n
  | LocalVar(_s,_) ->
      gen_addr node output;
      Printf.fprintf output "  mov (%%rax), %%rax\n"
  | BinOp({value=Assign; _}, l, r) ->
    gen_addr l output;
    push output dc;
    gen_expr r output dc;
    pop "%rdi" output dc;
    Printf.fprintf output "  mov %%rax, (%%rdi)\n"
  | BinOp(op, l, r) ->
    gen_expr r output dc;
    push output dc;
    gen_expr l output dc;
    pop "%rdi" output dc;
    begin match op.value with
    | Add  -> Printf.fprintf output "  add %%rdi, %%rax\n"
    | Sub  -> Printf.fprintf output "  sub %%rdi, %%rax\n"
    | Mult -> Printf.fprintf output "  imul %%rdi, %%rax\n"
    | Div  -> Printf.fprintf output "  cqo\n";
              Printf.fprintf output "  idiv %%rdi\n";
    | Eq   -> Printf.fprintf output "  cmp %%rdi, %%rax\n";
              Printf.fprintf output "  sete %%al\n";
              Printf.fprintf output "  movzx %%al, %%rax\n"
    | Ne   -> Printf.fprintf output "  cmp %%rdi, %%rax\n";
              Printf.fprintf output "  setne %%al\n";
              Printf.fprintf output "  movzx %%al, %%rax\n"
    | Lt   -> Printf.fprintf output "  cmp %%rdi, %%rax\n";
              Printf.fprintf output "  setl %%al\n";
              Printf.fprintf output "  movzx %%al, %%rax\n";
    | Le   -> Printf.fprintf output "  cmp %%rdi, %%rax\n";
              Printf.fprintf output "  setle %%al\n";
              Printf.fprintf output "  movzx %%al, %%rax\n"
    | _ -> failwith "error"
    end
  | _ -> failwith "error"

let gen_stmt node output dc =
    match node.value with
    | UniOp(_op, l) -> gen_expr l output dc
    | _ -> failwith "invalid statement"

let codegen program output =
  let dc = ref 0 in  
  (* Printf.fprintf output ".intel_syntax noprefix\n"; *)
  Printf.fprintf output ".globl _main\n";
  Printf.fprintf output "_main:\n";
  (* Prologue *)
  Printf.fprintf output "  push %%rbp\n";      (* ベースポインタを保存 *)
  Printf.fprintf output "  mov %%rsp, %%rbp\n"; (* ベースポインタに関数に入った時のスタックポインタを保存 *)
  Printf.fprintf output "  sub $208, %%rsp\n"; (* 変数の領域確保　26文字×8byte = 208byte *)
  Printf.fprintf output "\n";
  program |> List.iter (fun node ->
    gen_stmt node output dc;
    assert(!dc = 0);
    Printf.fprintf output "\n"
  );
  Printf.fprintf output "\n";
  Printf.fprintf output "  mov %%rbp, %%rsp\n"; (* スタックポインタの復元 *)
  Printf.fprintf output "  pop %%rbp\n";       (* ベースポインタの復元 *)
  Printf.fprintf output "  ret\n"
