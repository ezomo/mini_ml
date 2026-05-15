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

let rec run_test (src, expected, expected_ty) =
  let exp = parse src in
  let ty = indentify exp [] 0 in
  let value = eval exp [] in
  let result = string_of_value value in
  Printf.printf "Src    : %s\n" src;
  Printf.printf "Expect : %s : %s\n" expected expected_ty;
  Printf.printf "Result : %s : %s\n" result (string_of_ty ty);
  Printf.printf "\n"

let test_normal =
  [
    ("false", "false", "bool");
    ("true", "true", "bool");
    ("10", "10", "int");
    ("-100", "-100", "int");
    ("1+2", "3", "int");
    ("2 - 1", "1", "int");
    ("2*2", "4", "int");
    ("3 = 3", "true", "bool");
    ("3 != 4", "true", "bool");
    ("3 < 4", "true", "bool");
    ("3 > 2", "true", "bool");
    ("3 <= 3", "true", "bool");
    ("4 >= 5", "false", "bool");
    ("!true", "false", "bool");
    ("true && false", "false", "bool");
    ("true || false", "true", "bool");
    ("if true then 5 else 10", "5", "int");
    ("if false then 5 else 10", "10", "int");
    ("fun x -> x + 1", "λx.(x + 1)", "fun");
    ("(fun x->x+1)5", "6", "int");
    ("let x = 3 in x + 2", "5", "int");
    ("let f = fun x -> x + 1 in f 5", "6", "int");
  ]

let test_rec =
  [
    ( "let rec fact = fun n -> if n = 0 then 1 else n * fact (n - 1) in fact 5",
      "120",
      "int" );
    ( "let rec fib = fun n -> if n = 0 then 0 else if n = 1 then 1 else fib (n \
       - 1) + fib (n - 2) in fib 10",
      "55",
      "int" );
    ( "let rec add = fun x -> fun y -> if y = 0 then x else add (x + 1) (y - \
       1) in add 3 4",
      "7",
      "int" );
  ]

let test_poly =
  [
    ("let id = fun x -> x in let a = id 3 in id (fun y -> y)", "λy.y", "a -> a");
    ( "let rec id = fun x -> x in let a = id 3 in id (fun y -> y)",
      "λy.y",
      "a -> a" );
  ]

let test_z =
  (* let rec z f = f (fun x -> (z f) x) *)
  let z = "let rec z  = fun f -> f (fun x -> (z f) x) in " in
  [
    (z ^ "z", "λf.(λx.f (z f) x)", "(('a -> 'b) -> 'a -> 'b) -> 'a -> 'b");
    ( z ^ "z (fun g -> fun n -> if n = 0 then 1 else n * g (n - 1)) 5",
      "120",
      "int" );
  ]

(* let rec y f = f (fun x -> (y f) x) *)
let y = "let rec y  = fun f -> f (y f) in "

let () =
  print_endline "=== Normal Tests ===";
  List.iter run_test test_normal;

  print_endline "=== Recursive Function Tests ===";
  List.iter run_test test_rec;

  print_endline "=== Polymorphic Function Tests ===";
  List.iter run_test test_poly;

  print_endline "=== Z Combinator Tests ===";
  List.iter run_test test_z;

  print_endline "=== Y Combinator Tests ===";
  run_test (y ^ "y", "λf.(y f)", "(a -> a) -> a")

(* let () =
  run_test
    ( y ^ "y (fun g -> fun n -> if n = 0 then 1 else n * g (n - 1)) 5",
      "120",
      "int" ) *)
