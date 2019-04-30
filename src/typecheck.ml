(* Test *)
(* https://github.com/SOwens/example-compiler  - guide in compilers in OCaml *)
exception Lookup_failure
open Types

let rec update x y = function
  | (x', y')::l ->
    if x = x' then (x, y)::l
    else (x', y')::update x y l
  | _ -> raise Lookup_failure;;

let rec check_term (env: ident list) (funs: (ident * (int * bool)) list) : term -> string list = function
  | Var(x) -> if List.mem x env then []
              else [x ^ " not defined"]
  | Func(f, args) ->
    (match List.assoc_opt f funs with
     | None -> [f ^ " not defined"]
     | Some((n_args, data_fun)) ->
       if List.length args <> n_args then
       ["Wrong number of parameters in " ^ Types.show_term (Func(f,args))]
       else []) @ List.concat (List.map (check_term env funs) args)
  | Tuple(l) ->
      List.concat(List.map (check_term env funs) l)
  | Eq(x, y) | And(x,y) | Or(x,y) ->
      check_term env funs x @ check_term env funs y
  | Not(x) ->
      check_term env funs x

let rec check
  (g: global_type)
  (env : env_t)    (* each princ. with their known var, as a list *)
  (def : (ident * (env_t * global_type)) list)               (* function name, it's env and the global type (fenv) *)
  (funs : (ident * (int * bool)) list)           (* function name, number of args, data type *)
  : (string * global_type) list                    (* error messages and where in kode *)
   =

match g with
| Send(p, q, {authentic = a; secret = s}, x, t, g') ->
begin
  match List.assoc_opt p env with
  | None -> ["Principal " ^ p ^ " not defined", g]
  | Some(env_p) ->
    List.map (fun x -> (x, g)) (check_term env_p funs t) @
    match List.assoc_opt q env with
    | None -> ["Principal " ^ q ^ " not defined", g]
    | Some(env_q) ->
      let env' = update q (x::env_q) env in
      check g' env' def funs
end

| _ -> []
