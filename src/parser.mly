%{
open Ast
%}

%token QUESTION "?" COLON ":"
%token SAUF SI ALORS SINON
%token EQ "=" NEQ "<>" LTE "<=" LT "<" GT ">" GTE ">="
%token ET "ET" OU "OU" PAS "PAS"
%token PLUS "+" MOINS "-" MULT "*" DIV "/"
%token PG "(" PD ")"
%token AG "{" AD "}"
%token IMPRIME ENTREE  FIN REM COMMA
%token DEF SOUSROUTINE RETOURNE
%token TANTQUE
%token POUR DE JUSQUE
%token EOF SEMI ";"
%token <string> STRING
%token <string> VAR
%token <int> INT

%nonassoc ALORS
%nonassoc SINON

%nonassoc "?"
%nonassoc ":"

%left "OU"
%left "ET"
%nonassoc "PAS"
%nonassoc "=" "<>" "<=" "<" ">" ">="
%left "+" "-"
%left "*" "/"

%start<Ast.programme> main
%start<Ast.instruction> main_ligne
%%

let main := ~=programme; EOF; <>
let main_ligne := ~=instr; EOF; <>

let programme := ~=bloc; <Programme>

let bloc := ~=separated_list(";", instr); <>

let instr :=
 | "{"; ~=bloc; "}"; <Bloc>
 | REM; ~=STRING; <Rem>
 | FIN; {Fin}
 | IMPRIME; ~=separated_nonempty_list(COMMA, expr); <Imprime>
 | ENTREE; ~=var_list; <Entree>
 | DEF; ~=var; "{"; ~=bloc; "}"; <Proc>
 | SOUSROUTINE; ~=var; <Call>
 | RETOURNE; { Return }
 | ~ = instr_loop; <>
 | ~ = instr_assign; <>
 | ~ = instr_cond; <>


let instr_loop :=
 | TANTQUE; ~=expr; "{"; ~=bloc; "}"; <TantQue>
 | POUR; ~=var; DE; a=expr; JUSQUE; b=expr; "{"; ~=bloc; "}"; { Bloc [
        Let [ var, a ];
        TantQue ( Binop(EVar var, Lte, b), [
            Bloc bloc;
            Let [ var, Binop(EVar var, Plus, Int 1) ]] )
    ] }

let instr_cond :=
 | SI; ~=expr; ALORS; ~=instr; { SiAlorsSinon(expr, instr, None) }
 | SAUF; SI; ~=expr; ALORS; ~=instr; { SiAlorsSinon(Unop(UNot, expr), instr, None) }
 | SI; ~=expr; ALORS; x=instr; SINON; y=instr; { SiAlorsSinon(expr, x, Some y) }
 | SAUF; SI; ~=expr; ALORS; x=instr; SINON; y=instr; { SiAlorsSinon(Unop(UNot, expr), x, Some y) }

let instr_assign :=
 | v=var_list; "="; e=expr_list; { Let(List.combine v e) }
 | ~=var; ~=binop; "="; ~=expr; { Let([var, Binop(EVar var, binop, expr)]) }

let expr :=
 | ~=var; <EVar>
 | "("; ~=expr; ")"; <>
 | ~=INT; <Int>
 | ~=STRING; <String>
 | ~=unop; ~=expr; <Unop>
 | e1=expr; ~=relop; e2=expr; { Binop(e1, relop, e2) }
 | e1=expr; ~=binop; e2=expr; { Binop(e1, binop, e2) }
 | cond = expr; "?"; x = expr; ":"; y = expr; <Cond>

%inline var_list: v = separated_nonempty_list(COMMA, var) {v}
%inline expr_list: v = separated_nonempty_list(COMMA, expr) {v}

%inline unop:
  | "+" { UPlus }
  | "-" { UMinus }
  | "PAS" { UNot }

%inline relop:
  | "<>" { Neq } | "<=" { Lte } | "<" { Lt }
  | ">" { Gt }   | ">=" { Gte } | "=" { Eq }
  | "ET" { And } | "OU" { Or }

%inline binop :
  | "+" { Plus } | "-" { Minus }
  | "*" { Mul }  | "/" { Div }

let var := ~=VAR; <Var>
