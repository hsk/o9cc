%{
open Ast
let a v =
  {value = v; loc = (0, 0)}
let frame = ref []
let find_lvar name frame =
  List.assoc name frame
let id x =
    if List.mem_assoc x !frame then a@@LocalVar(x,find_lvar x !frame) else
    let offset = ((List.length !frame) + 1) * 8 in
    frame := !frame @ [(x,offset)];
    a@@LocalVar(x,offset)

%}
%token<int> INTEGER
%token<string> ID
%token EOF LPAREN "(" RPAREN ")" LBRACE "{" RBRACE "}" SEMI ";" EQ "="
%token STAR "*" PLUS "+" MINUS "-" DIV "/" LT "<" GT ">" LE "<=" GE ">=" EQEQ "==" NE "!="
%token RETURN "return" IF "if" ELSE "else"

%nonassoc NO_ELSE
%nonassoc ELSE 

%type<frame * program> translation_unit
%start translation_unit
%%
translation_unit:
| stmt_list EOF                           { !frame, $1 }

stmt_list:
| stmt                                    { [$1] }
| stmt stmt_list                          { $1::$2 }

stmt:
| expr_stmt                               { $1 }
| "return" expr ";"                       { a@@UniOp(a@@NdReturn,$2) }
| compound_stmt                           { $1 }
| if_stmt                                 { $1 }
| ";"                                     { a@@Block([]) }

expr_stmt:
| expr ";"                                { a@@UniOp(a@@NdExprStmt,$1) }

compound_stmt:
| "{" stmt_list "}"                       { a@@Block($2) }

if_stmt:
| "if" "(" expr ")" stmt %prec NO_ELSE    { a@@If($3,$5,a@@Block([])) }
| "if" "(" expr ")" stmt "else" stmt      { a@@If($3,$5,$7) }

primary_expr:
| INTEGER                                 { a@@Num $1 }
| ID                                      { id $1 }
| "(" expr ")"                            { $2 }

unary_expr:
| "+" primary_expr                        { $2 }
| "-" primary_expr                        { a@@BinOp(a@@Sub,a@@Num 0,$2) }
| primary_expr                            { $1 }

multiplicative_expr:
| unary_expr                              { $1 }
| multiplicative_expr "*" unary_expr      { a@@BinOp(a@@Mult,$1,$3) }
| multiplicative_expr "/" unary_expr      { a@@BinOp(a@@Div,$1,$3) }

additive_expr:
| multiplicative_expr                     { $1 }
| additive_expr "+" multiplicative_expr   { a@@BinOp(a@@Add,$1,$3) }
| additive_expr "-" multiplicative_expr   { a@@BinOp(a@@Sub,$1,$3) }

relational_expr:
| additive_expr                           { $1 }
| relational_expr "<" additive_expr       { a@@BinOp(a@@Lt,$1,$3) }
| relational_expr ">" additive_expr       { a@@BinOp(a@@Lt,$3,$1) }
| relational_expr "<=" additive_expr      { a@@BinOp(a@@Le,$1,$3) }
| relational_expr ">=" additive_expr      { a@@BinOp(a@@Le,$3,$1) }

equality_expr:
| relational_expr                         { $1 }
| equality_expr "==" relational_expr      { a@@BinOp(a@@Eq,$1,$3) }
| equality_expr "!=" relational_expr      { a@@BinOp(a@@Ne,$1,$3) }
assign_expr:
| equality_expr                           { $1 }
| equality_expr "=" assign_expr           { a@@BinOp(a@@Assign,$1,$3)}
expr:
| assign_expr                           { $1 }
