
open Ast

module GenCnt = struct
  let depth_add d dc =
    let (depth,label) = !dc in
    dc := (depth + d, label)
  let depth_sub d dc =
    let (depth,label) = !dc in
    dc := (depth - d, label)
  let label_up dc =
    let (depth,label) = !dc in
    dc := (depth, label + 1);
    label
end

let push output dc =
    Printf.fprintf output "  push %%rax\n";
    GenCnt.depth_add 1 dc

let pop s output dc =
    Printf.fprintf output "  pop %s\n" s;
    GenCnt.depth_sub 1 dc

let rec gen_expr node output dc =
  match node.value with
  | Num(n) ->
    Printf.fprintf output "  mov $%d, %%rax\n" n
  | LocalVar(_s,_) ->
      gen_addr node output dc;
      Printf.fprintf output "  mov (%%rax), %%rax\n"
  | BinOp({value=Assign; _}, l, r) ->
    gen_addr l output dc;
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
  | UniOp({value=Deref; _}, l) ->
    Printf.fprintf output "# deref\n";
    gen_expr l output dc;
    Printf.fprintf output "  mov (%%rax), %%rax\n"
  | UniOp({value=Addr; _}, l) ->
    Printf.fprintf output "# addr\n";
    gen_addr l output dc
  | _ -> failwith "GEN: Invalid expression."
and gen_addr node output dc =
  match node.value with
  | LocalVar(_, offset) ->
    Printf.fprintf output "  lea %d(%%rbp), %%rax\n" offset
  | UniOp({value=Deref; _}, l) -> gen_expr l output dc
  | _ ->
    failwith (Printf.sprintf "GEN: not an lvalue %s." (Ast.show_ast node))

let is_empty_block a =
  match a.value with
  | Block([]) -> true
  | _ -> false
let rec gen_stmt node output dc =
    match node.value with
    | UniOp(op, l) ->
      begin match op.value with
      | NdReturn ->
          gen_expr l output dc;
          Printf.fprintf output "  jmp .L.return\n"
      | NdExprStmt -> gen_expr l output dc
      | _ -> failwith "invalid statement"
      end
    | Block(body) ->
      body |> List.iter (fun node ->
        gen_stmt node output dc
      )
    | If(cond, thn, els) ->
      let c = GenCnt.label_up dc in
      gen_expr cond output dc;
      Printf.fprintf output "  cmp $0, %%rax\n";
      Printf.fprintf output "  je  .L.else.%d\n" c;
      gen_stmt thn output dc;
      Printf.fprintf output "  jmp .L.end.%d\n" c;
      Printf.fprintf output ".L.else.%d:\n" c;
      gen_stmt els output dc;
      Printf.fprintf output ".L.end.%d:\n" c;
    | For(init,cond,inc,body) ->
      let c = GenCnt.label_up dc in
      if not (is_empty_block init) then (
        gen_expr init output dc;
      );
      Printf.fprintf output ".L.begin.%d:\n" c;
      if not (is_empty_block cond) then (
          gen_expr cond output dc;
          Printf.fprintf output "  cmp $0, %%rax\n";
          Printf.fprintf output "  je  .L.end.%d\n" c;
      );
      gen_stmt body output dc;
      if not (is_empty_block inc) then 
          gen_expr inc output dc;
      Printf.fprintf output "  jmp .L.begin.%d\n" c;
      Printf.fprintf output ".L.end.%d:\n" c;
    | _ -> failwith "invalid statement"

let codegen program frame output =
  let dc = ref (0,0) in
  let stack_size = (List.length frame) * 8 in
  (* Printf.fprintf output ".intel_syntax noprefix\n"; *)
  Printf.fprintf output ".globl _main\n";
  Printf.fprintf output "_main:\n";
  (* Prologue *)
  Printf.fprintf output "  push %%rbp\n";      (* ベースポインタを保存 *)
  Printf.fprintf output "  mov %%rsp, %%rbp\n"; (* ベースポインタに関数に入った時のスタックポインタを保存 *)
  Printf.fprintf output "  sub $%d, %%rsp\n" stack_size; (* 変数の領域確保 *)
  Printf.fprintf output "\n";
  program |> List.iter (fun node ->
    gen_stmt node output dc;
    assert(fst !dc = 0);
    Printf.fprintf output "\n"
  );
  Printf.fprintf output "\n";
  Printf.fprintf output ".L.return:\n";
  Printf.fprintf output "  mov %%rbp, %%rsp\n"; (* スタックポインタの復元 *)
  Printf.fprintf output "  pop %%rbp\n";       (* ベースポインタの復元 *)
  Printf.fprintf output "  ret\n"
