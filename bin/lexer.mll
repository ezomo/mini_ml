{
  open Parser
  exception LexError of string
}

let white  = [' ' '\t' '\n' '\r']+
let digit  = ['0'-'9']
let int    = '-'? digit+
let letter = ['a'-'z' 'A'-'Z' '_']
let ident  = letter (letter | digit)*

rule token = parse
  | white       { token lexbuf }
  | "let"       { LET }
  | "rec"       { REC }
  | "in"        { IN }
  | "fun"       { FUN }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "="         { EQ }
  | "->"        { ARROW }
  | "+"         { PLUS }
  | "("         { LPAREN }
  | ")"         { RPAREN }
  | int  as n   { INT (int_of_string n) }
  | ident as s  { VAR s }
  | eof         { EOF }
  | _ as c      { raise (LexError (Printf.sprintf "unexpected char: %c" c)) }
