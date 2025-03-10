open Parser_plaf.Ast
open Parser_plaf.Parser
open Ds

(** [eval_expr e] evaluates expression [e] *)
let rec eval_expr : expr -> exp_val ea_result =
  fun e ->
  match e with
  | Int(n) ->
    return (NumVal n)
  | Var(id) ->
    apply_env id
  | Add(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1+n2))
  | Sub(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1-n2))
  | Mul(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1*n2))
  | Div(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    if n2==0
    then error "Division by zero"
    else return (NumVal (n1/n2))
  | Let(id,def,body) ->
    eval_expr def >>= 
    extend_env id >>+
    eval_expr body 
  | ITE(e1,e2,e3) ->
    eval_expr e1 >>=
    bool_of_boolVal >>= fun b ->
    if b 
    then eval_expr e2
    else eval_expr e3
  | IsZero(e) ->
    eval_expr e >>=
    int_of_numVal >>= fun n ->
    return (BoolVal (n = 0))
  | Debug(_e) ->
    string_of_env >>= fun str ->
    print_endline str; 
    error "Debug called"
  | IsEmpty ( e ) ->
    eval_expr e >>= fun ev ->
    (match ev with
    | TreeVal Empty -> return (BoolVal True)
    | TreeVal _ -> return (BoolVal False)
    | _ -> Error "Improper input, IsEmpty requires a tree")
  | EmptyTree ( _t ) ->
    return (TreeVal Empty)
  | Node ( e1 , e2 , e3 ) ->
    eval_expr e1 >>= fun n1 ->
    eval_expr e2 >>= fun n2 ->
    eval_expr e3 >>= fun n3 ->
    (match (n2, n3) with
    | (TreeVal t1, TreeVal t2) -> return (TreeVal (Node(n1, n2, n3)))
    | _ -> Error "2nd and 3rd arguments must be value tree values, Empty or a tree")
  | CaseT ( e1 , e2 , id1 , id2 , id3 , e3 ) ->
    eval_expr e1 >>= fun ev ->
    (match ev with
    | TreeVal Empty -> eval_expr e2
    | TreeVal (Node(v, left, right)) ->
      extend_env_list [id1; id2; id3] [v; TreeVal left; TreeVal right] >>+
      eval_expr e3
    | _ -> error "Expected a tree!")
  | _ -> failwith "Not implemented yet!"

(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e

(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : exp_val result =
  let c = e |> parse |> eval_prog
  in run c
  


