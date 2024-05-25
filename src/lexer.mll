{
open Parser

exception Error of string
}

let layout = [ ' ' '\t' '\n' ]

rule token = parse
  | layout    { token lexbuf }
  | eof      { EOF }
  | '\n' eof { EOF }
  | "SI"      { SI }
  | "SAUF"    { SAUF }
  | "ALORS"   { ALORS }
  | "SINON"   { SINON }
  | "?" { QUESTION }
  | ":" { COLON }
  | ";" { SEMI }
  | "{" { AG }
  | "}" { AD }
  | "ET"   { ET }
  | "OU"   { OU }
  | "PAS"   { PAS }
  | "ALORS"   { ALORS }
  | "IMPRIME" { IMPRIME }
  | "ENTREE"  { ENTREE }
  | "DEF"  { DEF }
  | "SOUSROUTINE"  { SOUSROUTINE }
  | "RETOURNE"  { RETOURNE }
  | "TANT QUE" { TANTQUE }
  | "POUR" { POUR }
  | "DE" { DE }
  | "JUSQUE" { JUSQUE }
  | "FIN" { FIN }
  | "REM" { REM }
  | "="  { EQ }
  | "+"  { PLUS }
  | "-"  { MOINS }
  | "*"  { MULT }
  | "/"  { DIV }
  | ","  { COMMA }
  | "<>" { NEQ }
  | "><" { NEQ }
  | "<=" { LTE }
  | "<"  { LT }
  | ">"  { GT }
  | ">=" { GTE }
  | "("  { PG }
  | ")"  { PD }
  | (['A'-'Z''a'-'z']* as s) { VAR(s) }
  |'"' ([^'"']* as s) '"' { STRING s }
  | ['0'-'9']+ as n       { INT (int_of_string n) }
  | _                     { raise @@ Error "unexpected character" }
