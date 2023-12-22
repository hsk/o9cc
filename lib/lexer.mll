{
open Parser
let keywords = [
  "return",RETURN;
  "if",IF;
  "else",ELSE;
  "for",FOR;
]
}
let space = [' ' '\t' '\r']
let ident = ['a'-'z'] ['0'-'9' 'a'-'z']*
rule token = parse
  | [' ' '\t' '\r' '\n'] { token lexbuf }
  | ['0'-'9']+ as n { INTEGER (int_of_string n) }
  | "+"   { PLUS }
  | "-"   { MINUS }
  | "*"   { STAR }
  | "/"   { DIV }
  | "("   { LPAREN }
  | ")"   { RPAREN }
  | "{"   { LBRACE }
  | "}"   { RBRACE }
  | "<"   { LT }
  | ">"   { GT }
  | "<="  { LE }
  | ">="  { GE }
  | "=="  { EQEQ }
  | "!="  { NE }
  | ";"   { SEMI }
  | "="   { EQ }
  | ident as n { try List.assoc n keywords
                 with _ -> ID n }
  | eof   { EOF }
