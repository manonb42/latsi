type programme = Programme of ligne list
and ligne = Ligne of int * instruction

and instruction =
    Bloc of instruction list
  | Imprime of expr list
  | SiAlorsSinon of expr * instruction * instruction option
  | VaVers of expr
  | Entree of var list
  | Let of (var * expr) list
  | Fin
  | NL
  | Rem of string
  | Call of expr
  | Return

and expr =
  | EVar of var
  | Int of int
  | String of string
  | Unop of unop * expr
  | Binop of expr * binop * expr
  | Cond of expr * expr * expr

and var = Var of string

and unop = UPlus | UMinus | UNot
and binop = Plus | Minus | Mul | Div | Modulo
          | Neq | Lte | Lt | Gt | Gte | Eq
          | And | Or
