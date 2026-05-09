(* 型変数 *)
type tyvar = string

(* 単相型 *)
type ty = TInt | TArrow of ty * ty | TVar of tyvar

(* 多相型　しょうみ [多相許可変数] * 変数を含む型 *)
type scheme = Forall of tyvar list * ty

(* 変数名と実際の型 *)
type tyenv = (tyvar * scheme) list

let typver_new n = (TVar ("'a" ^ string_of_int n), n + 1)
