%{
open Ast
let a v =
    {value = v; loc = (0, 0)}
%}
%token<int> INTEGER
%token EOF LPAREN "(" RPAREN ")"
%token STAR "*" PLUS "+" MINUS "-" DIV "/"
%type<ast> translation_unit
%start translation_unit
%%
translation_unit:
| additive_expr EOF                       { $1 }

primary_expr:
| INTEGER                                 { a@@Num $1 }
| "(" additive_expr ")"                   { $2 }

multiplicative_expr:
| primary_expr                            { $1 }
| multiplicative_expr "*" primary_expr    { a@@BinOp(a@@Mult,$1,$3) }
| multiplicative_expr "/" primary_expr    { a@@BinOp(a@@Div,$1,$3) }

additive_expr:
| multiplicative_expr                     { $1 }
| additive_expr "+" multiplicative_expr   { a@@BinOp(a@@Add,$1,$3) }
| additive_expr "-" multiplicative_expr   { a@@BinOp(a@@Sub,$1,$3) }
