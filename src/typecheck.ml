(* Test *)
(* https://github.com/SOwens/example-compiler  - guide in compilers in OCaml *)

  let check e =
    match e with
    | BinOp "+" e1 e2 -> let t1 = check e1 in
                         let t2 = check e2 in
                         if t1 : int && t2 = int then
    return int else throw_exception
    | _ -> ...



let rec update x y = function
  | (x', y')::l ->
    if x = x' then (x, y)::l
    else (x', y')::update x y l

let rec check_term env funs = function
  | Var(x) -> if List.mem x env then []
              else [x ^ " not defined"]
  | Func(f, args) ->
    (match List.assoc f funs with
     | Some((n_args, data_fun)) ->
       if List.length args <> n_args then
       ["Wrong number of parameters in " ^ show_term (Func(f,args))]
       else []) @ List.concat (List.map (check_term env funs) args)
  | Tuple(l) ->
      List.concat(List.map (check_term env funs) l)
  | Eq(x, y) | And(x,y) | Or(x,y) ->
      check_term env funs x @ check_term env funs y
  | Not(x) ->
      check_term env funs x

let rec check
  (g: G)
  (env : (principal, var list) list)    (* each princ. with their known var, as a list *)
  (def : (fname, (env, G)))               (* function name, it's env and the global type (fenv) *)
  (funs : (fname, (int, bool)))           (* function name, number of args, data type *)
  : (string, G) list                    (* error messages and where in kode *)
   =

match g with
| Send(p, q, {a, s}, x, t, g') ->
begin
  match List.assoc p env with
  | None -> ["Principal " ^ p ^ " not defined", g]
  | Some(env_p) ->
    List.map (fun x -> (x, g)) (check_term env_p funs t) @
    match List.assoc q env with
    | None -> ["Principal " ^ q ^ " not defined", g]
    | Some(env_q) ->
      let env' = update q (x::env_q) env in
      check g' env' defs funs
end
| Global_type (loc, term) ->
    let inferred = infer p xenv loc hyps tenv term in
    if entailment hyps [(expected, inferred)] = false then
      mismatch xenv loc hyps expected inferred

| _ ->
    (* out of luck! *)
    assert false

(* Type error message ('a = for all types 'a) *)
let type_error (ln : int option) (msg : string) : 'a =
  match ln with
  | Some ln ->
    raise (BadInput ("Type error on line " ^ string_of_int ln ^ ": " ^ msg))
  | None ->
    raise (BadInput ("Type error at unknown location: " ^ msg))
    in

let var_lookup = Context.var_lookup handle_error in

let validate_fun_call context fun_name arg_count =
  match var_lookup context fun_name with
  | Context.Fun i ->
     if i <> arg_count then
       handle_error "called function with wrong number of arguments"
     else ()
  | _ -> handle_error "tried to call identifier that isn't a function"
in
