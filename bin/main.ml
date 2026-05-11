open Ast
open Eval
open Infer
open Lib
open Visualize

let run exp =
  try
    let ty = identify exp [] 0 in
    let value = eval exp [] in

    let st_exp = string_of_exp exp in
    let st_ty = string_of_ty ty in
    let st_value = string_of_value value in

    print_endline st_exp;
    print_endline st_ty;
    print_endline st_value
  with Failure msg -> print_endline ("Error: " ^ msg)

let poly_test =
  Let
    ( "id",
      Fun ("x", Var "x"),
      Let ("a", App (Var "id", Int 3), App (Var "id", Fun ("y", Var "y"))) )

let () = run poly_test
