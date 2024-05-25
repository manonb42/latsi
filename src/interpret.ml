open Ast;;


type exit_reason =
  | Finished
  | EndOfInput

exception Exit of exit_reason;;
exception TypeError;;
let type_error () = raise TypeError

module Vars = Map.Make (String);;

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
  line: int;
  vars: value Vars.t;
  stack: int list;
}

let default_ctx = { line = 0;  vars = Vars.empty; stack = []}


let get_var ctx (Var v) = try Vars.find v ctx.vars with Not_found -> failwith @@ "undefined variable : '" ^ v ^ "'"
let set_var ctx (Var v) x = { ctx with vars = Vars.add v x ctx.vars }
let set_line ctx line = { ctx with line }
let push ctx n = { ctx with stack = n::ctx.stack }
let pop ctx = match ctx.stack with
  | x::xs -> { ctx with stack = xs }, x
  | _ -> raise Not_found

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

let rec execute ctx (i: instruction) =
  let next ctx = set_line ctx (ctx.line + 1)  in
  match i with
| Imprime l -> List.iter (fun v -> print_value @@ eval ctx v) l; next ctx
| SiAlorsSinon (cond, x, y) ->
    if to_bool @@ eval ctx cond then execute ctx x
    else Option.map (execute ctx) y |> Option.value ~default:(next ctx)
| VaVers line ->  set_line ctx (to_int @@ eval ctx line) ;
| Entree vs -> List.fold_left
                 (fun ctx v -> VInt (read_int ()) |> set_var ctx v )
                 ctx vs |> next
| Let vs -> List.map (fun (v, e) -> (v, eval ctx e)) vs
            |> List.fold_left (fun ctx (v,x) -> set_var ctx v x) ctx
            |> next
| Fin -> raise (Exit Finished)
| NL -> print_newline (); next ctx
| Rem _ -> next ctx
| Call l ->
    let target = to_int @@ eval ctx l in
    let ctx = push ctx (next ctx).line in
    set_line ctx target
| Return ->
  let ctx, line = pop ctx in
  set_line ctx line
| Bloc instr ->
  let rec aux ctx instr = match instr with
    | [] -> ctx
    | x::xs -> match execute ctx x with
      | ctx' when ctx'.line <> ctx.line + 1 -> ctx'
      | ctx' when xs=[] -> ctx'
      | ctx' -> aux (set_line ctx' ctx.line) xs
  in aux ctx instr
;;


let next_line cur (Programme p) =
  List.fold_left (fun acc (Ligne (m, _) as l2 ) -> match acc with
      | _ when m < cur -> acc
      | Some (Ligne (n,_)) when n < m -> acc
      | _ -> Some l2
    ) None p



let step (p: programme) ctx  =
    let Ligne(n, instr) = match next_line ctx.line p with
      | Some v ->  v
      | None -> raise (Exit EndOfInput)
    in let ctx = set_line ctx n in
    execute ctx instr
;;

let run (p: programme) =
  let rec aux ctx =
      try aux @@ step p ctx with Exit Finished -> ()
  in aux default_ctx
;;
