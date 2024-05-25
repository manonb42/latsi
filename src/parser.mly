%{
open Ast
%}


%token SI ALORS
%token EQ NEQ LTE LT GT GTE
%token PLUS MOINS MULT DIV
%token IMPRIME VAVERS ENTREE PG PD FIN REM NL COMMA
%token EOF CR
%token<string> STRING
%token <char> VAR
%token <int> INT
%start<Ast.programme> input

%%

var: v=VAR { Var v }
relop:
  | NEQ { NEQ }
  | LTE { LTE }
  | LT { LT }
  | GT { GT }
  | GTE { GTE }
  | EQ { EQ }


input: p=programme EOF { p }

programme: l=separated_nonempty_list(CR, n=INT i=instruction {Ligne (n, i)}) { Programme l }

instruction:
| IMPRIME s=expr_list { Imprime s}
| SI e1=expression op=relop e2=expression ALORS i=instruction { Si_Alors (e1, op, e2, i) }
| VAVERS e=expression { Vavers e }
| ENTREE vs=var_list { Entree vs }
| v=VAR EQ e=expression { Let (Var v,e) }
| FIN { Fin }
| REM s=STRING { Rem s }
| NL { NL }


expr_list: l = separated_nonempty_list(COMMA, expr_or_string) { l }

expr_or_string:
    | s = STRING     { String s }
    | e = expression { Expression_s_or_e e }

var_list: l = separated_nonempty_list(COMMA, var) { l }


expression:
| t=terme { (Plus t) :: [] }
| t=terme l=operateur_terme_list { (Plus t) :: l }
| PLUS t=terme l=operateur_terme_list { (Plus t) :: l }
| MOINS t=terme l=operateur_terme_list { (Moins t) :: l }


operateur_terme_list:
  | op=operateur_terme { [op] }
  | PLUS t=terme l=operateur_terme_list { (Plus t) :: l }
  | MOINS t=terme l=operateur_terme_list { (Moins t) :: l }

operateur_terme:
 | t=terme { Plus t }
 | PLUS t=terme { Plus t }
 | MOINS t=terme { Moins t }

terme:
  |f=facteur fl=op_facteur_list* {Terme (f, fl)}

op_facteur_list:
  |MULT f=facteur { (Mult, f) }
  |DIV f=facteur { (Div, f) }

facteur:
  |v=VAR { Var_f(Var v) }
  |n=INT { Nombre_f(n) }
  |PG e=expression PD { Expression_f(e) }
