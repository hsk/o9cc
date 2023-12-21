%{
open Ast
let a v =
    {value = v; loc = (0, 0)}
%}
%token<int> INTEGER
%token EOF LPAREN "(" RPAREN ")"
%token STAR "*" PLUS "+" MINUS "-" DIV "/" LT "<" GT ">" LE "<=" GE ">=" EQEQ "==" NE "!="
%type<ast> translation_unit
%start translation_unit
%%
translation_unit:
| expr EOF                                { $1 }

primary_expr:
| INTEGER                                 { a@@Num $1 }
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

expr:
| equality_expr                           { $1 }
