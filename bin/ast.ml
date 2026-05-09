(* stringのままだと、なんか気に食わなかった *)
type var = string

type exp =
  | Let of var * exp * exp
  | Fun of var * exp
  | App of exp * exp
  | Var of var
  | Plus of exp * exp
  | Int of int

type value = VInt of int | VClosure of var * exp * env
and env = (var * value) list
