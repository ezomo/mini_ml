(* stringのままだと、なんか気に食わなかった *)
type var = string

type exp =
  | Let of var * exp * exp
  | Fun of var * exp
  | LetRec of var * var * exp * exp (* 関数名,変数名 , 関数本体, in以降*)
  | App of exp * exp
  | Var of var
  | Plus of exp * exp
  | Int of int
  | Bool of bool
  | IF of exp * exp * exp

type value =
  | VInt of int
  | VBool of bool
  | VClosure of var * exp * env (* 変数名, 関数本体, 環境 *)
  | VRecClosure of var * var * exp * env (* 関数名, 変数名, 関数本体, 環境 *)

and env = (var * value) list
