open Ast
open Eval
open Infer
open Lib
open Visualize

let parse (src : string) : Ast.exp =
  let lexbuf = Lexing.from_string src in
  Parser.prog Lexer.token lexbuf

let run exp =
  try
    let ty = indentify exp [] 0 in
    let value = eval exp [] in

    Printf.printf "Expr  : %s\n" (string_of_exp exp);
    Printf.printf "Type  : %s\n" (string_of_ty ty);
    Printf.printf "Value : %s\n" (string_of_value value);
    Printf.printf "\n"
  with Failure msg -> print_endline ("Error: " ^ msg)

let poly_test = parse "let id = fun x -> x in let a = id 3 in id (fun y -> y)"
let () = run poly_test
let if_test = parse "3 + (if true then 5 else 10)"
let () = run if_test
let eq_test = parse "3 = 3"
let () = run eq_test
let rec_test = parse "let rec add = fun x -> if x = 0 then 0 else x in add "
let () = run rec_test
let rec f = 2
