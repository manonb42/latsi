type var = Var of char

and relop = NEQ | LTE | LT | GT | GTE | EQ

and expression = operateur_terme list
and operateur_terme = Plus of terme | Moins of terme

and terme = Terme  of facteur * ((op_facteur * facteur) list)
and op_facteur = Mult | Div
and facteur = Var_f of var |Nombre_f of int | Expression_f of expression

and var_list = var list

and expr_list = string_or_exp list
and string_or_exp = String of string | Expression_s_or_e of expression

and instruction =
    Imprime of expr_list
  | Si_Alors of expression * relop * expression * instruction
  | Vavers of expression
  | Entree of var_list
  | Let of var * expression
  | Fin
  | NL
  | Rem of string

and ligne = Ligne of int * instruction

and programme = Programme of ligne list


let rec as_string : programme -> string = function
|Programme [] -> ""
|Programme (hd :: tl) -> as_string_l hd ^ "\n" ^ as_string (Programme tl)

and as_string_l = function
  |Ligne (n, instr) -> string_of_int n ^ " " ^ (as_string_instr instr)



and as_string_p = function
    | [] -> "EOF"
  | hd :: tl -> (as_string_l hd) ^ "\n" ^ (as_string_p tl)

and as_string_facteur = function
    |Var_f Var v -> String.make 1 v
    |Nombre_f (n) -> string_of_int n
    |Expression_f e -> as_string_exp e

and as_string_facteur_l : (op_facteur * facteur) list -> string = function
    |[] -> ""
    |(Mult, hd) :: tl -> " * " ^ as_string_facteur hd ^ as_string_facteur_l tl
    |(Div, hd) :: tl -> " / " ^ as_string_facteur hd ^ as_string_facteur_l tl

and as_string_terme = function
    | Terme ((f1), f2) -> as_string_facteur f1 ^ (as_string_facteur_l f2)

and as_string_op_terme = function
  |Plus t -> " + " ^ as_string_terme t
  |Moins t -> " - " ^ as_string_terme t

and as_string_exp = function
  |[] -> ""
  |hd :: tl -> as_string_op_terme hd ^ as_string_exp tl

and as_string_s_or_e = function
  |String s -> "\"" ^ s ^ "\""
  |Expression_s_or_e e -> as_string_exp e

and as_string_exp_l = function
    | [] -> ""
    | hd :: tl -> as_string_s_or_e hd ^ " , " ^ as_string_exp_l tl

and as_string_var_l = function
  | Var x::y::xs -> (String.make 1 x) ^ ", " ^ as_string_var_l (y::xs)
  | Var x::[] -> (String.make 1 x)
  | _ -> ""

and as_string_relop = function
  |NEQ -> " <> "
  |LTE -> " <= "
  |LT -> " < "
  |GT -> " > "
  |GTE -> " >= "
  |EQ -> " = "

and as_string_instr : instruction -> string = function
    | Imprime l -> "IMPRIME : " ^ (as_string_exp_l l)
    | Si_Alors (exp, relop, exp2, instr) ->
            " SI " ^ (as_string_exp exp) ^ (as_string_relop relop) ^ (as_string_exp exp2) ^ " ALORS " ^ (as_string_instr instr)
    | Vavers exp -> "VAVERS " ^ as_string_exp exp
  | Entree var_l -> "ENTREE : " ^ as_string_var_l var_l 
  | Let (Var var,exp) -> (String.make 1 var) ^ " = " ^ (as_string_exp exp)
  | Fin -> "FIN"
  | Rem s -> "REM " ^ s
  | NL -> "NL"


and to_nombre n = Nombre_f n
