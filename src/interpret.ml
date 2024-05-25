open Ast;;


type exit_reason =
  | Finished
  | EndOfInput

exception Exit of exit_reason;;



type context = {
  line: int;
  vars: int array
}

let var_index (Var v) = int_of_char v - int_of_char 'A'
let get_var ctx v = ctx.vars.(var_index v)
let set_var ctx v x = ctx.vars.(var_index v) <- x; ctx
let set_line ctx line = { ctx with line }



let rec eval ctx (e: expression) =
  let aux = function Plus t -> eval_terme ctx t
                   | Moins t -> -eval_terme ctx t
  in List.fold_left (fun acc t -> acc + aux t) 0 e

and eval_terme ctx (Terme (f, ts)) =
  let apply_op = function Div -> (/) | Mult -> ( * )
  in List.fold_left (fun acc (op,f) ->
      apply_op op acc (eval_facteur ctx f))
    (eval_facteur ctx f) ts


and eval_facteur ctx (f: facteur) = match f with
| Var_f v -> get_var ctx v
| Nombre_f n -> n
| Expression_f e -> eval ctx e
;;

let rec execute ctx (i: instruction) =
  let next ctx = set_line ctx (ctx.line + 1)  in
  match i with
| Imprime l -> List.iter (function
      | String s -> print_string s
      | Expression_s_or_e e -> eval ctx e |> print_int) l; next ctx
| Si_Alors (x, op, y, branch) ->
    let x,y = eval ctx x, eval ctx y
    and cond = match op with
    | NEQ -> (<>) | LTE -> (<=) | LT -> (<)
    | GT -> (>) | GTE -> (>=) | EQ -> (=)
    in if cond x y then execute ctx branch
       else next ctx
| Vavers line ->  set_line ctx (eval ctx line) ;
| Entree vs -> List.fold_left
                 (fun ctx v -> read_int () |> set_var ctx v )
                 ctx vs |> next
| Let (v, x) -> set_var ctx v (eval ctx x) |> next
| Fin -> raise (Exit Finished)
| NL -> print_newline (); next ctx
| Rem _ -> next ctx
;;

let next_line cur (Programme p) =
  List.fold_left (fun acc (Ligne (m, _) as l2 ) -> match acc with
      | _ when m < cur -> acc
      | Some (Ligne (n,_)) when n < m -> acc
      | _ -> Some l2
    ) None p

let step (p: programme) ctx  =
    let Ligne(n, instr) = match next_line ctx.line p with
      | Some v -> v
      | None -> raise (Exit EndOfInput)
    in let ctx = set_line ctx n
    in let ctx = execute ctx instr
    in ctx
;;

let run (p: programme) =
  let ctx = { line = 0;  vars = Array.make 26 0 }
  in let rec aux ctx =
       try aux @@ step p ctx with Exit Finished -> ()
  in aux ctx
;;
