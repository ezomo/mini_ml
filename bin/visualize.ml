open Types
open Ast

let rec string_of_exp exp =
  let string_of_func (arg_name, body) =
    "λ" ^ arg_name ^ "." ^ string_of_exp body
  in
  match exp with
  | Plus (e1, e2) -> "(" ^ string_of_exp e1 ^ " + " ^ string_of_exp e2 ^ ")"
  | Int this -> string_of_int this
  | Bool this -> string_of_bool this
  | Fun (x, body) -> string_of_func (x, body)
  | App (e1, e2) -> "(" ^ string_of_exp e1 ^ " " ^ string_of_exp e2 ^ ")"
  | Var name -> name
  | Let (x, e1, e2) ->
      "(let " ^ x ^ " = " ^ string_of_exp e1 ^ " in " ^ string_of_exp e2 ^ ")"
  | IF (e1, e2, e3) ->
      "(if " ^ string_of_exp e1 ^ " then " ^ string_of_exp e2 ^ " else "
      ^ string_of_exp e3 ^ ")"
  | LetRec (fn_name, (arg_name, fn_body), in_exp) ->
      "(let rec " ^ fn_name ^ "= "
      ^ string_of_func (arg_name, fn_body)
      ^ " in " ^ string_of_exp in_exp ^ ")"
  | Eq (e1, e2) -> "(" ^ string_of_exp e1 ^ " = " ^ string_of_exp e2 ^ ")"

let string_of_value v =
  let rec string_of_vfunc (arg_name, body, env) =
    "λ" ^ arg_name ^ "." ^ string_of_exp body
  in
  match v with
  | VInt x -> string_of_int x
  | VBool x -> string_of_bool x
  | VClosure (name, exp, env) -> string_of_vfunc (name, exp, env)
  | VRecClosure (fn_name, vf) -> "rec " ^ fn_name ^ " " ^ string_of_vfunc vf
(* env は危険そう *)

let rec string_of_ty ty =
  match ty with
  | TInt -> "int"
  | TBool -> "bool"
  | TArrow (a, b) -> string_of_ty a ^ " -> " ^ string_of_ty b
  | TVar this -> this

let print_constraint (t1, t2) =
  print_endline (string_of_ty t1 ^ " = " ^ string_of_ty t2)
