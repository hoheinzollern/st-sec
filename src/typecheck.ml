(* https://github.com/SOwens/example-compiler  - guide in compilers in OCaml *)

exception Lookup_failure
open Types

let rec update x y = function
  | (x', y')::l ->             (* a::[b,c] = [a,b,c] add item to the beginning of a list *)
    if x = x' then (x, y)::l
    else (x', y')::update x y l
  | _ -> raise Lookup_failure;;
  (* env' = update q (x::env_q) env *)

let rec check_term (env: ident list) (funs: (ident * (int * bool)) list) : term -> string list = function
  | Var(x) -> if List.mem x env then [] (* List.mem x -> if x exists = true *)
              else [x ^ " not defined"]
  | Func(f, args) ->
    (match List.assoc_opt f funs with (* asooc_opt = returns the value associated with key a in the list of pairs l *)
     | None -> [f ^ " not defined"]
     | Some((n_args, data_fun)) ->
       if List.length args <> n_args then (* checks if number of args match (<> not equal) *)
       ["Wrong number of parameters in " ^ Types.show_term (Func(f,args))]
       else []) @ List.concat (List.map (check_term env funs) args)
  | Tuple(l) ->
      List.concat(List.map (check_term env funs) l) (* recursively checks terms with their env and funcs, concat = flattens map *)
  | Eq(t1, t2) | And(t1, t2) | Or(t1, t2) ->
      check_term env funs t1 @ check_term env funs t2
  | Not(t) ->
      check_term env funs t

(* Or can I just use ref to term? *)
let rec check_pattern env funs = function
  | PVar(x) -> if List.mem x env then []
               else [x ^ " not defined in pattern"]
  | PMatch(t) ->
      check_term env funs t
  | PFunc(f, args) ->
      (match List.assoc_opt f funs with
        | None -> [f ^ " not defined"]
        | Some((n_args, data_fun)) ->
          if List.length args <> n_args then
          ["Wrong number of parameters in " ^ show_pattern(PFunc(f, args))]
          else []) @ List.concat (List.map (check_pattern env funs) args)
  | PTuple(l) ->
      List.concat(List.map (check_pattern env funs) l)

let rec check_let_bind p env_p env funs = function
  | New(x, lb) ->
    let env' = update p (x::env_p) env in (* update env of p *)
    check_let_bind p env_p env' funs lb
  | Let(x, t, lb) ->
    check_term env funs t @
    let env' = update p (x::env_p) env in (* update env of p *)
    check_let_bind p env_p env' funs lb
    (* check term, update env, check let_bind on body *)
  | LetEnd -> env'


let rec check
  (g : global_type)                             (* Global type *)
  (env : tenv)                                  (* each princ. with their known var, as a list *)
  (def : (ident * (tenv * global_type)) list)   (* function name, it's env and the global type (fenv) *)
  (funs : (ident * (int * bool)) list)          (* function name, number of args, data type *)
  : (string * global_type) list                 (* error messages and where in code *)
   =

match g with
| Send(p, q, {authentic = a; secret = s}, x, t, g') -> (* check p,q, check t is well-formed, check G princ.*)
begin   (* does p and q exist in the env *)
  match List.assoc_opt p env with                  (* returns ident list of p in env *)
  | None -> ["Principal " ^ p ^ " not defined", g]
  | Some(env_p) ->                                 (* env_p = the env associated to p *)
    List.map (fun x -> (x, g)) (check_term env_p funs t) @ (* checks wether the terms exist in p's env *)
    match List.assoc_opt q env with                 (* returns ident list of q in env *)
    | None -> ["Principal " ^ q ^ " not defined", g]
    | Some(env_q) ->
      let env' = update q (x::env_q) env in         (* update env of q *)
      check g' env' def funs
end
(* TODO: recursively check pattern is well-formed, check G_i princ(add env[q] fu(pattern_i))*)
| Branch(p, q, {authentic = a; secret = s}, t, args) -> (* args = pattern * global type *)
begin
  match List.assoc_opt p env with
  | None -> ["Princepal " ^ p ^ " not defined", g]
  | Some(env_p) ->
    List.map (fun x -> (x, g)) (check_term env_p funs t) @ (* fun x.. : for return type error (message, G) *)
    match List.assq_opt q env with
    | None -> ["Principal " ^ q ^ " not defined", g]
    | Some(env_q) ->
      (* check pattern is well-formed, add pattern to q' env *)
      (* free variables: fv(x) = {x} *)
end
| Compute(p, lb, g') ->
begin (* done in if statement instead? *)
  match List.assoc_opt p env with
  | None -> ["Principal " ^ p ^ " not defined", g]
  | Some(env_p) ->
    let env' = check_let_bind p env_p env funs lb in
    check g' env' def funs
end
(*
| DefGlobal(x, y list, g', g'') ->
| CallGlobal(x, t list) ->
*)

| GlobalEnd() -> [] (* needed? *)
| _ -> [] (* if nothing, return empty list of errors *)
