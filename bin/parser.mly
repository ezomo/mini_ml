%{
  open Ast
%}

%token <int>    INT
%token <string> VAR
%token LET  REC IN FUN IF THEN ELSE TRUE FALSE
%token EQ ARROW PLUS LPAREN RPAREN EOF

(* 優先順位：低い順に書く *)
(*  %nonassoc IN ELSE
%nonassoc ARROW
%left PLUS
%left APP   (* 関数適用 *)
*)

%start <Ast.exp> prog
%%

prog:
  | e = expr EOF  { e }

expr:
  | LET REC f = VAR EQ FUN x = VAR ARROW e = expr IN e2 = expr { LetRec (f, (x, e), e2) }
  | LET x = VAR EQ e1 = expr IN e2 = expr                      { Let (x, e1, e2) }
  | FUN x = VAR ARROW e = expr                                 { Fun (x, e) }
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr                 { IF (e1, e2, e3) }
  | e = eq_expr                                                { e }                          



eq_expr:
  | e1 = eq_expr EQ e2 = add_expr  { Eq (e1, e2) }
  | e = add_expr                   { e }

add_expr:
  | e1 = add_expr PLUS e2 = app_expr  { Plus (e1, e2) }
  | e = app_expr                      { e }

app_expr:
  | e1 = app_expr e2 = atom_expr  { App (e1, e2) }
  | e = atom_expr                 { e }

atom_expr:
  | n = INT              { Int n }
  | TRUE                 { Bool true }
  | FALSE                { Bool false }
  | x = VAR              { Var x }
  | LPAREN e = expr RPAREN  { e }
