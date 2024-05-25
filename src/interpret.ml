open Ast;;


type exit_reason =
  | Finished
  | EndOfInput

exception Exit of exit_reason;;
exception TypeError;;
let type_error () = raise TypeError

module Env = Map.Make (String);;

type value =
    VInt of int
  | VStr of string

let to_int v = match v with | VInt n -> n | _ -> type_error ()
let to_str v = match v with | VStr n -> n | _ -> type_error ()
let to_bool  = function
  | VInt n -> n<>0
  | VStr s -> s<>""
let of_bool b = VInt (if b then 1 else 0)
let print_value v = match v with
  | VInt n -> print_int n
  | VStr s -> print_string s

type context = {
  vars: value Env.t;
  runstack: bloc list;
  procs: bloc Env.t
}

let default_ctx p = { vars = Env.empty; runstack = [p]; procs = Env.empty}


let get_var ctx (Var v) = try Env.find v ctx.vars with Not_found -> failwith @@ "undefined variable : '" ^ v ^ "'"
let set_var ctx (Var v) x = { ctx with vars = Env.add v x ctx.vars }
let get_proc ctx (Var v) = try Env.find v ctx.procs with Not_found -> failwith @@ "undefined proc : '" ^ v ^ "'"
let set_proc ctx (Var v) x = { ctx with procs = Env.add v x ctx.procs }

let push ctx b = { ctx with runstack = b::ctx.runstack }
let pop ctx = match ctx.runstack with
  | [] -> raise (Exit EndOfInput)
  | b::blocs -> b, { ctx with runstack = blocs }
let rec next ctx = match pop ctx with
    | [], ctx -> next ctx
    | i::is, ctx -> i, push ctx is

let apply_op (op : binop) v1 v2 =
  let apply int_op str_op error = match v1, v2 with
    | VInt x, VInt y -> int_op x y
    | VStr x, VStr y -> str_op x y
    | _ -> error v1 v2
  and vint op x y =  VInt (op x y) and vstr op x y =  VStr (op x y)
  and vbool op x y = of_bool (op x y)
  and bad _ _ = type_error ()
  and log op =  of_bool @@ op (to_bool v1) (to_bool v2)
  in match op with
  | Plus -> apply (vint (+)) (vstr (^)) bad
  | Minus -> apply (vint (-)) bad bad
  | Mul -> apply (vint ( * )) bad bad
  | Div -> apply (vint (/)) bad bad
  | Modulo -> apply (vint (mod)) bad bad
  | Neq ->  apply (vbool (<>)) (vbool (<>)) bad
  | Lte ->  apply (vbool (<=)) (vbool (<=)) bad
  | Lt  ->  apply (vbool (<)) (vbool (<)) bad
  | Gt  ->  apply (vbool (>)) (vbool (>)) bad
  | Gte ->  apply (vbool (>=)) (vbool (>=)) bad
  | Eq  ->  apply (vbool (=)) (vbool (=)) bad
  | And -> log (&&)
  | Or  ->  log (||)

let rec eval ctx e = match e with
| EVar v -> get_var ctx v
| Int i -> VInt i
| String s -> VStr s
| Unop (UPlus, e) -> eval ctx e
| Unop (UMinus, e) -> VInt(-(to_int (eval ctx e)))
| Unop (UNot, e) -> of_bool (not (to_bool (eval ctx e)))
| Binop (x, op, y) -> apply_op op (eval ctx x) (eval ctx y)
| Cond (cond, x, y) -> eval ctx (if to_bool @@ eval ctx cond then x else y)

let rec execute ctx' (i: instruction) =
  let ctx = snd @@ next ctx' in
  match i with
| Imprime l -> List.iter (fun v -> print_value @@ eval ctx v) l; ctx
| SiAlorsSinon (cond, x, y) ->
    if to_bool @@ eval ctx cond then execute ctx x
    else Option.map (execute ctx) y |> Option.value ~default:(ctx)
| Entree vs -> List.fold_left
    (fun ctx v -> VInt (read_int ()) |> set_var ctx v )
    ctx vs
| Let vs -> List.map (fun (v, e) -> (v, eval ctx e)) vs
            |> List.fold_left (fun ctx (v,x) -> set_var ctx v x) ctx
| Fin -> raise (Exit Finished)
| NL -> print_newline (); ctx
| Rem _ -> ctx
| Call v -> push ctx @@ get_proc ctx v
| Proc (v,b)  -> set_proc ctx v b
| Return -> snd @@ pop ctx
| Bloc [] -> ctx
| Bloc (i::_ as bloc) -> execute (push ctx bloc) i
| TantQue (cond, bloc) ->
    if to_bool @@ eval ctx cond
    then push ctx' bloc else ctx
;;


let step_in ctx = execute ctx (fst @@ next ctx) ;;

let step_over ctx =
  let depth ctx = List.length ctx.runstack in
  let l = depth ctx in
  let rec aux ctx =
    let ctx = step_in ctx in
    if l <= depth ctx then ctx
    else aux ctx
  in aux ctx
;;

let run (Programme p) =
  let rec aux ctx =
      try aux @@ step_over ctx with Exit Finished -> ()
  in aux (default_ctx p)
;;

