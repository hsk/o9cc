{
open Parser
}
let space = [' ' '\t' '\r']

rule token = parse
  | [' ' '\t' '\r' '\n'] { token lexbuf }
  | ['0'-'9']+ as n { INTEGER (int_of_string n) }
  | "+" { OPERATOR("+") }
  | "-" { OPERATOR("-") }
  | eof { EOF }