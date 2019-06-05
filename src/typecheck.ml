(* https://github.com/SOwens/example-compiler  - guide in compilers in OCaml *)

exception Lookup_failure
open Types

let rec update x y = function
  | (x', y')::l ->             (* a::[b,c] = [a,b,c] add item to the beginning of a list *)
    if x = x' then (x, y)::l
    else (x', y')::update x y l
  | _ -> raise Lookup_failure;;
  (* env' = update q (x::env_q) env *)

let check_func f args funs =
  match List.assoc_opt f funs with
    | None -> [f ^ " not defined"]
    | Some((n_args, data_fun)) ->
      if List.length args <> n_args then
      ["Wrong number of parameters in " ^ f]
      else [] @
      if not data_fun then [f ^ " is not a data function"] else []

let rec check_term (env: ident list) (funs: (ident * (int * bool)) list) : term -> string list = function
  | Var(x) -> if List.mem x env then [] (* List.mem x -> if x exists = true *)
              else [x ^ " not defined"]
  | Func(f, args) ->
    check_func f args funs
  (*  (match List.assoc_opt f funs with (* asooc_opt = returns the value associated with key a in the list of pairs l *)
     | None -> [f ^ " not defined"]
     | Some((n_args, data_fun)) ->
       if List.length args <> n_args then (* checks if number of args match (<> not equal) *)
       ["Wrong number of parameters in " ^ Types.show_term (Func(f,args))]
       else []) *)
      @ List.concat (List.map (check_term env funs) args)
  | Tuple(l) ->
      List.concat(List.map (check_term env funs) l) (* recursively checks terms with their env and funcs, concat = flattens map *)
  | Eq(t1, t2) | And(t1, t2) | Or(t1, t2) ->
      check_term env funs t1 @ check_term env funs t2
  | Not(t) ->
      check_term env funs t


let rec check_pattern env funs = function
  | PVar(x) -> if not (List.mem x env) then []
               else [x ^ " already defined in pattern"]
  | PMatch(t) ->
      check_term env funs t
  | PFunc(f, args) ->
    check_func f args funs
    (* (match List.assoc_opt f funs with
        | None -> [f ^ " not defined"]
        | Some((n_args, data_fun)) ->
          if List.length args <> n_args then
          ["Wrong number of parameters in " ^ show_pattern(PFunc(f, args))]
          else [] @
          if not data_fun then [f ^ " is not a data function"] else []) *)
          @ List.concat (List.map (check_pattern env funs) args)
  | PTuple(l) ->
      List.concat(List.map (check_pattern env funs) l)

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

(*
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
      check_branches args env def funs
    (*
      List.concat (List.map (
        fun (p, g) -> ((check_pattern env_q funs p), g)   (* pattern and global type *)
        ) args) @ *)

      (* check pattern is well-formed, add pattern to q' env *)
      (* free variables: fv(x) = {x} *)
end
*)

| Compute(p, lb, g') ->
begin
  match List.assoc_opt p env with
  | None -> ["Principal " ^ p ^ " not defined", g]
  | Some(env_p) ->
    let rec let_bind env_p' env' = function
      | New(x, lb') ->
        let env_p'' = (x::env_p') in
        let_bind env_p'' (update p env_p'' env') lb'
      | Let(x, t, lb') ->
        List.map (fun x -> (x, g)) (check_term env_p' funs t) @
        let env_p'' = (x::env_p') in
        let_bind env_p'' (update p env_p'' env') lb'
      | LetEnd -> check g' env' def funs
    in let_bind env_p env lb
end

(*
| DefGlobal(f, args, g', g'') ->
  ((x, (n, bool)) :: funs) (* update funs with new function *)
  (*(def : (ident * (tenv * global_type)) list)     function name, it's env and the global type (fenv) *)
*)

| CallGlobal(f, args) -> (* check number of args and data type *)
  List.map (fun x -> (x, g)) (check_func f args funs) @
  check g env def funs

| GlobalEnd -> []
| _ -> [] (* if nothing, return empty list of errors *)

(*
and check_branches
  (args : (pattern * global_type) list)         (* branches *)
  (env : tenv)                                  (* each princ. with their known var, as a list *)
  (def : (ident * (tenv * global_type)) list)   (* function name, it's env and the global type (fenv) *)
  (funs : (ident * (int * bool)) list)          (* function name, number of args, data type *)
  : (string * global_type) list                 (* error messages and where in code *)
= match args with
| [] -> []
| ((pattern, global_type)::args') ->

    check_pattern g @
    check_branches args' env def funs
*)
