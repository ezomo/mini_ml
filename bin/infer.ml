open Ast
open Types
open Lib

let rec occurs tyvar ty =
  match ty with
  | TInt -> false
  | TArrow (i, o) -> occurs tyvar i || occurs tyvar o
  | TVar ty -> tyvar = ty

let rec subst_ty ob_ty (tyvar, ac_ty) =
  match ob_ty with
  | TInt -> TInt
  | TArrow (t2, t3) ->
      TArrow (subst_ty t2 (tyvar, ac_ty), subst_ty t3 (tyvar, ac_ty))
  | TVar name -> if name = tyvar then ac_ty else TVar name

let subst_eqs (tyvar, ac_ty) eqs =
  List.map
    (fun (t1, t2) -> (subst_ty t1 (tyvar, ac_ty), subst_ty t2 (tyvar, ac_ty)))
    eqs

let compose_subst (tyvar, ty) theta =
  let updated =
    List.map (fun (name, ty1) -> (name, subst_ty ty1 (tyvar, ty))) theta
  in
  (* 同じ型変数が既にあるなら無視 *)
  try
    let _ = lookup tyvar updated in
    updated
  with Failure _ -> (tyvar, ty) :: updated

(* 型内部の型変数 *)
let ftv_ty ty =
  let rec main ty list =
    match ty with
    | TVar tyvar -> tyvar :: list
    | TArrow (t1, t2) -> main t1 (main t2 list)
    | TInt -> list
  in
  main ty []

(* 型内部の自由型変数 *)

let ftv_scheme (Forall (vars, ty)) = diff (ftv_ty ty) vars

(* 環境の自由型変数 *)
let rec ftv_env env =
  match env with
  | [] -> []
  | (_, scheme) :: rest -> ftv_scheme scheme @ ftv_env rest

let rec unify eql theta =
  match eql with
  | [] -> theta
  (* a = a　<-無意味 *)
  | (t1, t2) :: rest when t1 = t2 -> unify rest theta
  (* a -> b = e -> f =>　a = e ,b = f    *)
  | (TArrow (a1, a2), TArrow (b1, b2)) :: rest ->
      unify ((a1, b1) :: (a2, b2) :: rest) theta
  (* 'a = ty, 'aをtyで書き換え *)
  | (TVar a, t) :: rest ->
      if occurs a t then failwith "occurs check"
      else unify (subst_eqs (a, t) rest) (compose_subst (a, t) theta)
  | (t, TVar a) :: rest ->
      if occurs a t then failwith "occurs check"
      else unify (subst_eqs (a, t) rest) (compose_subst (a, t) theta)
  | _ -> failwith "unification failed"

(* 一般化されてる型変数を具体化 *)
let instantiate scheme n =
  let (Forall (tyvars, ty)) = scheme in
  List.fold_left
    (fun (ty, n) tyvar ->
      let fresh, n' = typver_new n in
      (subst_ty ty (tyvar, fresh), n'))
    (ty, n) tyvars

let subst_scheme (Forall (vars, ty)) (tyvar, ac_ty) =
  if List.exists (fun v -> v = tyvar) vars then Forall (vars, ty)
  else Forall (vars, subst_ty ty (tyvar, ac_ty))

let subst_scheme_env env thetas =
  List.map
    (fun (x, scheme) ->
      let scheme' = List.fold_left subst_scheme scheme thetas in
      (x, scheme'))
    env

let generalize env t =
  let vars = diff (ftv_ty t) (ftv_env env) in
  Forall (vars, t)

let rec inf env exp n =
  match exp with
  | Int _ -> (TInt, [], n)
  | Plus (e1, e2) ->
      let ty1, con1, n1 = inf env e1 n in
      let ty2, con2, n2 = inf env e2 n1 in
      (* t1 = t2 , t1 = int *)
      (TInt, (ty1, TInt) :: (ty2, TInt) :: (con1 @ con2), n2)
  | App (e1, e2) ->
      let t1, c1, n1 = inf env e1 n in
      let t2, c2, n2 = inf env e2 n1 in
      let tx, nx = typver_new n2 in
      (* t1 = t2 -> tx *)
      (tx, (t1, TArrow (t2, tx)) :: (c1 @ c2), nx)
  | Fun (name, e1) ->
      let tx, nx = typver_new n in
      (* λx.e ; x:tx ,e:te , tx -> te *)
      let fn_env = (name, Forall ([], tx)) :: env in
      let te, ce, n1 = inf fn_env e1 nx in
      (TArrow (tx, te), ce, n1)
  | Var var ->
      let scheme = lookup var env in
      let ty, n1 = instantiate scheme n in
      (ty, [], n1)
  | Let (name, e1, e2) ->
      let t1, c1, n1 = inf env e1 n in
      let theta = unify c1 [] in
      let ty = List.fold_left subst_ty t1 theta in
      let env1 = subst_scheme_env env theta in
      let sigma = generalize env1 ty in
      let env2 = (name, sigma) :: env1 in
      inf env2 e2 n1

let indentify exp env n =
  let ty, eqs, _ = inf env exp n in
  let theta = unify eqs [] in
  List.fold_left subst_ty ty theta
