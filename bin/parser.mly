%{
  open Ast
%}

%token <int>    INT
%token <string> VAR
%token LET  REC IN FUN IF THEN ELSE TRUE FALSE
%token EQ NEQ LT GT LE GE PLUS MINUS TIMES 
%token AND OR NOT
%token ARROW
%token LPAREN RPAREN EOF

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
  | e = logical_or                                 { e }  




logical_or:
  | e1 = logical_or OR e2 = logical_and { Or (e1, e2) }
  | e = logical_and { e }

logical_and:
  | e1 = logical_and AND e2 = equality { And (e1, e2) }
  | e = equality { e }


(* equality        = relational [ ("==" | "!=") relational ] ; *)
(*繰り返しは1回までのはずなので上の式は間違ってる？*)
equality:
  | e1  = relational  EQ e2 = relational { Eq (e1, e2) }
  | e1  = relational  NEQ e2 = relational { Neq (e1, e2) }
  | e = relational { e }


relational:
  | e1 = relational LT e2 = additive { Lt (e1, e2) }
  | e1 = relational GT e2 = additive { Gt (e1, e2) }
  | e1 = relational LE e2 = additive { Le (e1, e2) }
  | e1 = relational GE e2 = additive { Ge (e1, e2) }
  | e = additive { e }


additive:
  | e1 = additive PLUS e2 = multiplicative { Plus (e1, e2) }
  | e1 = additive MINUS e2 = multiplicative { Sub (e1, e2) }
  | e = multiplicative { e }

multiplicative:
  | e1 = multiplicative TIMES e2 = application { Mul (e1, e2) }
  | e = application { e }

application:
  | e1 = application e2 = unary { App (e1, e2) }
  | e = unary { e }

unary:
  | NOT e = unary { Not e }
  | e = atom { e }

atom:
  | i = INT { Int i }
  | x = VAR   { Var x }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | LPAREN e = expr RPAREN { e }