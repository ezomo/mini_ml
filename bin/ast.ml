(* stringのままだと、なんか気に食わなかった *)
type var = string

type exp =
  | Let of var * exp * exp
  | Fun of func
  | LetRec of var * func * exp (* 関数名, 関数, in以降*)
  | App of exp * exp
  | Var of var
  | Plus of exp * exp
  | Int of int
  | Bool of bool
  | IF of exp * exp * exp
  | Eq of exp * exp

and func = var * exp

type value =
  | VInt of int
  | VBool of bool
  | VClosure of vfunc (* 関数 *)
  | VRecClosure of var * vfunc (* 関数名, 関数*)

and env = (var * value) list
and vfunc = var * exp * env (* 引数名, 関数本体, 定義されたときの環境 *)
