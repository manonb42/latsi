type programme = Programme of bloc

and bloc = instruction list

and instruction =
    Bloc of bloc
  | Imprime of expr list
  | TantQue of expr * bloc
  | SiAlorsSinon of expr * instruction * instruction option
  | Entree of var list
  | Let of (var * expr) list
  | Fin
  | NL
  | Rem of string
  | Proc of var * bloc
  | Call of var
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
