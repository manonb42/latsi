{
open Parser
}

let layout = [ ' ' '\t' ]

rule main = parse
  | layout    { main lexbuf }
  | '\n'     { CR }
  | eof      { EOF }
  | '\n' eof { EOF }
  | "SI"      { SI }
  | "IMPRIME" { IMPRIME }
  | "ALORS"   { ALORS }
  | "VAVERS"  { VAVERS }
  | "ENTREE"  { ENTREE }
  | "FIN" { FIN }
  | "REM" { REM }
  | "NL" { NL }
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
  | (['A'-'Z'] as x)      { VAR(x) }
  |'"' ([^'"']* as s) '"' { STRING s }
  | ['0'-'9']+ as n       { INT (int_of_string n) }
  | _                     { failwith "unexpected character" }
