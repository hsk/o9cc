
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

type uniOpKind = NdExprStmt | NdReturn
[@@deriving show]

type uniOp = uniOpKind annot
[@@deriving show]

type astKind =
  | Num of int
  | LocalVar of string * int
  | BinOp of binOp * ast * ast
  | UniOp of uniOp * ast
  | Block of ast list
  | If of ast * ast * ast
  | For of ast * ast * ast * ast
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
  let inc_write_node file cnt node =
    let cnt = cnt + 1 in
    let name = node_name cnt in
    Printf.fprintf file "%s -> %s\n" self_node_name name;
    write_node file cnt node
  in
  let write_label label =
    Printf.fprintf file "%s[label=%s]\n" self_node_name label
  in
  match node.value with
  | Num(n) ->
    write_label (string_of_int n);
    cnt
  | BinOp(op, l, r) ->
    let label = match op.value with
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
    write_label label;
    let cnt = inc_write_node file cnt l in
    let cnt = inc_write_node file cnt r in
    cnt
  | LocalVar(name, offset) ->
    write_label (Printf.sprintf "\"%s\n%d\"" name offset);
    cnt
  | UniOp(op, l) ->
    (match op.value with
    | NdExprStmt -> write_label "\"EXPR_STMT\""
    | NdReturn -> write_label "\"RETURN\""
    );
    inc_write_node file cnt l
  | Block(body) ->
    write_label "BLOCK";
    List.fold_left (inc_write_node file) cnt body
  | If(cond, thn, els) ->
    write_label "IF";
    let cnt = inc_write_node file cnt cond in
    let cnt = inc_write_node file cnt thn in
    let cnt = inc_write_node file cnt els in
    cnt
  | For(init, cond, inc, body) ->
    write_label "FOR";
    let cnt = inc_write_node file cnt init in
    let cnt = inc_write_node file cnt cond in
    let cnt = inc_write_node file cnt inc in
    let cnt = inc_write_node file cnt body in
    cnt

let write_dot program path =
  let file = open_out path in
  Printf.fprintf file "digraph G {{\n";
  program |> List.fold_left (write_node file) 0 |> ignore;
  Printf.fprintf file "}}\n"
