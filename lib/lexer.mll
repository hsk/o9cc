{
open Parser
}
let space = [' ' '\t' '\r']
let ident = ['a'-'z']+
rule token = parse
  | [' ' '\t' '\r' '\n'] { token lexbuf }
  | ['0'-'9']+ as n { INTEGER (int_of_string n) }
  | "+"   { PLUS }
  | "-"   { MINUS }
  | "*"   { STAR }
  | "/"   { DIV }
  | "("   { LPAREN }
  | ")"   { RPAREN }
  | "<"   { LT }
  | ">"   { GT }
  | "<="  { LE }
  | ">="  { GE }
  | "=="  { EQEQ }
  | "!="  { NE }
  | ";"   { SEMI }
  | "="   { EQ }
  | ident as n { ID n }
  | eof   { EOF }
