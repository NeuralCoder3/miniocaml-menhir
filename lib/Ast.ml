type op = Add | Sub | Mul | Leq
type ty = Int | Bool | Fun of ty * ty

type exp =
  | Icon of int
  | Bcon of bool
  | Var of string
  | Binary of op * exp * exp
  | If of exp * exp * exp
  | Let of string * exp * exp
  | Lam of string * ty * exp
  | App of exp * exp
  | Letrec of string * string * ty * ty * exp * exp

type program = 
  Expression of exp


let rec show_ty = function
  | Int -> "Int"
  | Bool -> "Bool"
  | Fun (t1, t2) -> Printf.sprintf "Fun (%s, %s)" (show_ty t1) (show_ty t2)

let show_op = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Leq -> "Leq"

let rec show_ast = function
  | Icon c -> Printf.sprintf "Icon %d" c
  | Bcon b -> Printf.sprintf "Bcon %b" b
  | Var x -> Printf.sprintf "Var %s" x
  | Binary (op, e1, e2) -> Printf.sprintf "Binary (%s, %s, %s)" (show_op op) (show_ast e1) (show_ast e2)
  | If (e1, e2, e3) -> Printf.sprintf "If (%s, %s, %s)" (show_ast e1) (show_ast e2) (show_ast e3)
  | Let (x, e1, e2) -> Printf.sprintf "Let (%s, %s, %s)" x (show_ast e1) (show_ast e2)
  | Lam (x, t, e) -> Printf.sprintf "Lam (%s, %s, %s)" x (show_ty t) (show_ast e)
  | App (e1, e2) -> Printf.sprintf "App (%s, %s)" (show_ast e1) (show_ast e2)
  | Letrec (f, x, t1, t2, e1, e2) -> Printf.sprintf "Letrec (%s, %s, %s, %s, %s, %s)" f x (show_ty t1) (show_ty t2) (show_ast e1) (show_ast e2)