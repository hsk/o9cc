
type loc = int * int
[@@derive show]

type 't annot = {value: 't; loc: loc}
[@@derive show]

type binOpKind =
  | Add
  | Sub
  | Mult
  | Div
  | Eq    (* == *)
  | Ne    (* != *)
  | Lt    (* <  *)
  | Le    (* <= *)

[@@derive show]

type binOp = binOpKind annot
[@@derive show]

type astKind =
  | Num of int
  | BinOp of binOp * ast * ast
[@@derive show]
and ast = astKind annot
[@@derive show]

let node_name cnt =
  Printf.sprintf "node%d" cnt

let rec write_node node file cnt =
  let self_node_name = node_name cnt in
  match node.value with
  | Num(n) ->
    Printf.fprintf file "%s[label=%d]\n" self_node_name n;
    cnt
  | BinOp(op, l, r) ->
    let cnt = write_node l file (cnt + 1) in
    let left_node_name = node_name cnt in
    let cnt = write_node r file (cnt + 1) in
    let right_node_name = node_name cnt in
    let op_str = match op.value with
      | Add  -> "\"+\""
      | Sub  -> "\"-\""
      | Mult -> "\"*\""
      | Div  -> "\"/\""
      | Eq   -> "\"==\""
      | Ne   -> "\"!=\""
      | Lt   -> "\"<\""
      | Le   -> "\"<=\""
    in
    Printf.fprintf file "%s[label=%s]\n" self_node_name op_str;
    Printf.fprintf file "%s -> %s\n" self_node_name left_node_name;
    Printf.fprintf file "%s -> %s\n" self_node_name right_node_name;
    cnt

let write_dot ast path =
  let file = open_out path in
  Printf.fprintf file "digraph G {{\n";
  write_node ast file 0 |> ignore;
  Printf.fprintf file "}}\n"
