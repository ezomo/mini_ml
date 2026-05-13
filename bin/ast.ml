(* stringのままだと、なんか気に食わなかった *)
type var = string

type exp =
  | Let of var * exp * exp
  | Fun of var * exp
  | App of exp * exp
  | Var of var
  | Plus of exp * exp
  | Int of int
  | Bool of bool
  | IF of exp * exp * exp

type value = VInt of int | VBool of bool | VClosure of var * exp * env
and env = (var * value) list
