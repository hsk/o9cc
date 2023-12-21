
type loc = int * int
[@@deriving show]

type 't annot = {value: 't; loc: loc}
[@@deriving show]

type binOpKind =
  | Add
  | Sub
  | Mult
  | Div
  | Eq    (* == *)
  | Ne    (* != *)
  | Lt    (* <  *)
  | Le    (* <= *)
  | Assign
[@@deriving show]

type binOp = binOpKind annot
[@@deriving show]

type uniOpKind = ND_EXPR_STMT | ND_RETURN
[@@deriving show]

type uniOp = uniOpKind annot
[@@deriving show]

type astKind =
  | Num of int
  | LocalVar of string * int
  | BinOp of binOp * ast * ast
  | UniOp of uniOp * ast
[@@deriving show]
and ast = astKind annot
[@@deriving show]

type program = ast list
[@@deriving show]

type localVariable = string * int
[@@deriving show]

type frame = localVariable list
[@@deriving show]

let node_name cnt =
  Printf.sprintf "node%d" cnt

let rec write_node file cnt node =
  let self_node_name = node_name cnt in
  match node.value with
  | Num(n) ->
    Printf.fprintf file "%s[label=%d]\n" self_node_name n;
    cnt
  | BinOp(op, l, r) ->
    let cnt = write_node file (cnt + 1) l in
    let left_node_name = node_name cnt in
    let cnt = write_node file (cnt + 1) r in
    let right_node_name = node_name cnt in
    let op_str = match op.value with
      | Add    -> "\"+\""
      | Sub    -> "\"-\""
      | Mult   -> "\"*\""
      | Div    -> "\"/\""
      | Eq     -> "\"==\""
      | Ne     -> "\"!=\""
      | Lt     -> "\"<\""
      | Le     -> "\"<=\""
      | Assign -> "\"=\""
    in
    Printf.fprintf file "%s[label=%s]\n" self_node_name op_str;
    Printf.fprintf file "%s -> %s\n" self_node_name left_node_name;
    Printf.fprintf file "%s -> %s\n" self_node_name right_node_name;
    cnt
  | LocalVar(name, offset) ->
    Printf.fprintf file "%s[label=\"%s\n%d\"]" self_node_name name offset;
    cnt
  | UniOp(op, l) ->
      let cnt = write_node file (cnt + 1) l in
      let left_node_name = node_name cnt in
      let op_str = match op.value with
          | ND_EXPR_STMT -> "\"EXPR_STMT\""
          | ND_RETURN -> "\"RETURN\""
      in
      Printf.fprintf file "%s[label=%s]" self_node_name op_str;
      Printf.fprintf file "%s -> %s" self_node_name left_node_name;
      cnt

let write_dot program path =
  let file = open_out path in
  Printf.fprintf file "digraph G {{\n";
  program |> List.fold_left (write_node file) 0 |> ignore;
  Printf.fprintf file "}}\n"
