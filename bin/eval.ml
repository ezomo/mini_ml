open Ast
open Lib

let rec eval exp env =
  match exp with
  | Plus (e1, e2) -> (
      let v1 = eval e1 env in
      let v2 = eval e2 env in
      match (v1, v2) with
      | VInt n1, VInt n2 -> VInt (n1 + n2)
      | _ -> failwith "type error")
  | Int this -> VInt this
  | Var name -> lookup name env
  | App (e1, e2) -> (
      let v1 = eval e1 env in
      let v2 = eval e2 env in
      match v1 with
      | VClosure (name, fn, env') -> eval fn ((name, v2) :: env')
      | _ -> failwith "not a function")
  | Fun (name, exp) -> VClosure (name, exp, env)
  | Let (name, e1, e2) ->
      let v1 = eval e1 env in
      eval e2 ((name, v1) :: env)
