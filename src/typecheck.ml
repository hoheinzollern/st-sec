(* https://github.com/SOwens/example-compiler  - guide in compilers in OCaml *)

exception Lookup_failure
open Types

(* Update list of pair with x and y, returns updated env *)
let rec update x y = function
  | (x', y')::l ->             (* a::[b,c] = [a,b,c] add item to the beginning of a list *)
    if x = x' then (x, y)::l
    else (x', y')::update x y l
  | _ -> raise Lookup_failure;;
  (* env' = update q (x::env_q) env *)

type resultOrError =
    Result of tenv
  | Error of (string * global_type) list

(* env, param *)
(* Error messages suppose to be return of check, and not in env *)
(* remove duplicates ?? *)
let union_env x y g : resultOrError =
  let sort_list = List.sort compare in
  let rec help_union xs ys (a:tenv) =
    match xs, ys with
      | [], [] -> Result (a)
      | [], (p, x)::xs -> Error (["Principal " ^ p ^ " not defined", g])
      | xs, [] -> Result ( a @ xs )
      | (p, x)::xs, ((p', y)::ys as param') ->
        if p = p' then help_union xs ys ((p, x@y)::a)
        else if p > p' then Error (["Principal " ^ p' ^ " not defined", g])
          else help_union xs param' ((p,x)::a)
    in help_union (List.sort compare x) (sort_list y) []
(*
let union_env x y : (string * a' list) list =
  let sort_list l =
    List.sort compare l in
  let rec help_union xs ys =
    match xs, ys with
      | [], [] -> []
      | [], (p, x)::xs -> ["Principal " ^ p ^ " not defined", []]
      | xs, [] -> xs
      | (p, x)::xs, ((p', y)::ys as param') ->
        if p = p' then (p, x@y)::help_union xs ys
        else if p > p' then ["Principal " ^ p' ^ " not defined", []]
          else (p, x)::help_union xs param')
    in help_union (sort_list x) (sort_list y)
*)
(*
let rec test xs ys =
  match xs, ys with
  | [], [] -> []
  | [], ys -> ys
  | xs, [] -> xs
  | (a, b)::xs, (c, d)::ys ->
    test xs ys ;;

let num = [("a", "y"); ("t", "a"); ("b", "b"); ("c", "e")]
let num = [("a", [4; 2]); ("d", [3]); ("b", [2; 4]); ("c", [2; 2; 5])]
let nums = [("a", [4; 2]); ("d", [3]); ("b", [2; 4]); ("c", [2; 2; 5])]
List.sort compare num;; nums;;
*)

(* Checks if function: exist, right number of args, if data func, return list of errors *)
let check_func f args funs =
  match List.assoc_opt f funs with
    | None -> [f ^ " not defined"]
    | Some((n_args, data_fun)) ->
      if List.length args <> n_args then
      ["Wrong number of parameters in " ^ f] (* Types.show_term (Func(f,args)) instead of f *)
      else [] @
      if not data_fun then [f ^ " is not a data function"] else []

(* Checks if term: exists, check_func, return list of errors *)
let rec check_term (env: ident list) (funs: (ident * (int * bool)) list) : term -> string list = function
  | Var(x) -> if List.mem x env then [] (* List.mem x -> if x exists = true *)
              else [x ^ " not defined"]
  | Func(f, args) ->
    check_func f args funs @
    List.concat (List.map (check_term env funs) args)
  | Tuple(l) ->
      List.concat(List.map (check_term env funs) l) (* recursively checks terms with their env and funcs, concat = flattens map *)
  | Eq(t1, t2) | And(t1, t2) | Or(t1, t2) ->
      check_term env funs t1 @ check_term env funs t2
  | Not(t) ->
      check_term env funs t

(* Checks if pattern: is not pre-defined, check_term, check_func, return list of errors *)
let rec check_pattern env funs = function
  | PVar(x) -> if not (List.mem x env) then []   (* check for free variables *)
               else [x ^ " already defined in pattern"]
  | PMatch(t) ->
      check_term env funs t
  | PFunc(f, args) ->
    check_func f args funs @
    List.concat (List.map (check_pattern env funs) args)
  | PTuple(l) ->
      List.concat(List.map (check_pattern env funs) l)

(* return env_p *)
(*let check_princ p env =
  match List.assoc_opt p env with
   | None -> ["Princepal " ^ p ^ " not defined", g]
   | Some(env_p) -> *)

(* Checks global types, return list of errors *)
let rec check
  (g : global_type)                             (* Global type *)
  (env : tenv)                                  (* each princ. with their known var, as a list *)
  (def : (ident * (tenv * global_type)) list)   (* function name, it's env and the global type *)
  (funs : (ident * (int * bool)) list)          (* function name, number of args, data type *)
  : (string * global_type) list                 (* error messages and where in code *)
   =

(* checks send: if p and q exist, if t is well-formed, updates env of q with x *)
match g with
| Send(p, q, {authentic = a; secret = s}, x, t, g') ->
begin
  match List.assoc_opt p env with                  (* returns ident list of p in env *)
  | None -> ["Principal " ^ p ^ " not defined", g]
  | Some(env_p) ->
    List.map (fun e -> (e, g)) (check_term env_p funs t) @ (* fun x.. : for return type error (message, G) *)
    match List.assoc_opt q env with
    | None -> ["Principal " ^ q ^ " not defined", g]
    | Some(env_q) ->
      let env' = update q (x::env_q) env in
      check g' env' def funs
end

(* checks branch: if p and q exist, if t is well-formed, recursively check patterns *)
| Branch(p, q, {authentic = a; secret = s}, t, args) -> (* args: pattern * global type *)
begin
  match List.assoc_opt p env with
  | None -> ["Princepal " ^ p ^ " not defined", g]
  | Some(env_p) ->
    List.map (fun e -> (e, g)) (check_term env_p funs t) @
    match List.assq_opt q env with
    | None -> ["Principal " ^ q ^ " not defined", g]
    | Some(env_q) ->
      List.concat (List.map (
        fun (p, g) -> List.map (fun e -> (e, g)) (check_pattern env_q funs p)   (* pattern and global type *)
        ) args) @
      List.concat (List.map (
        fun (p, g) -> check g env def funs) args)
        (*check_branches args env env_q def funs*)
end

(* Checks let-binding: update env of participant *)
| Compute(p, lb, g') ->
begin
  match List.assoc_opt p env with
  | None -> ["Principal " ^ p ^ " not defined", g]
  | Some(env_p) ->
    let rec let_bind env_p env = function
      | New(x, lb) ->
        let env_p' = (x::env_p) in
        let_bind env_p' (update p env_p' env) lb
      | Let(x, t, lb) ->
        List.map (fun e -> (e, g)) (check_term env_p funs t) @
        let env_p' = (x::env_p) in
        let_bind env_p' (update p env_p' env) lb
      | LetEnd -> check g' env def funs
    in let_bind env_p env lb
end

(* Checks function definition: *)
| DefGlobal(f, params, g', g'') ->
  let def' = ((f, (params, g'))::def) in
  let env' = union_env env params g in
  begin
    match env' with
      | Error(err) -> err
      | Result(env) -> (check g' env def' funs) @
                       (check g'' env def' funs)
  end


    (* env' = update q (x::env_q) env *)
(* check if princ exists? *)
  (* def: (ident * (env * global_type)) list)
  env: (princ * ident list) list *)

(* Checks function call: exists, number of args, if datatype*)
| CallGlobal(f, args) ->
  List.map (fun e -> (e, g)) (check_func f args funs) @
  check g env def funs
  (* check fv(t1) is a subset of the env of p *)

| GlobalEnd -> []
(*| _ -> [] *)

and check_branches
  (args : (pattern * global_type) list)         (* branches *)
  (env : tenv)                                  (* each princ. with their known var, as a list *)
  (env_q : ident list)                          (* env of q *)
  (def : (ident * (tenv * global_type)) list)   (* function name, it's env and the global type (fenv) *)
  (funs : (ident * (int * bool)) list)          (* function name, number of args, data type *)
  : (string * global_type) list                 (* error messages and where in code *)
= match args with
| [] -> []
| ((pattern, global_type)::args') ->
  check global_type env def funs @
  check_branches args' env env_q def funs
