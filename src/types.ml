
type principal = string
type ident = string

(* 1. Types *)

(* Terms *)
type term =
    Var of ident
  | Func of ident * term list
  | Tuple of term list
  | Eq of term * term
  | And of term * term
  | Or of term * term
  | Not of term

(* Let bindings *)
type let_bind =
    New of ident * let_bind
  | Let of ident * term * let_bind
  | LetEnd

(* Channel options / Bullet notation *)
type channel_options = { authentic: bool; secret: bool }

(* Global types: p -> q *)
type global_type =
    Send of principal * principal * channel_options * term * global_type
  | Branch of principal * principal * channel_options * term * (term * global_type) list
  | Compute of principal * let_bind * global_type
  | DefGlobal of ident * ident list * global_type * global_type
  | CallGlobal of ident * term list
  | GlobalEnd

(* 2. Should do when.. *)

(* Terms *)
let rec show_term = function
    Var(x) -> x
  | Func(name, args) -> name ^ "(" ^ show_term_list args ^ ")"
  | Tuple(args) -> "<" ^ show_term_list args ^ ">"
  | Eq(t1, t2) -> show_term t1 ^ " = " ^ show_term t2
  | And(t1, t2) -> show_term t1 ^ " & " ^ show_term t2
  | Or(t1, t2) -> show_term t1 ^ " | " ^ show_term t2
  | Not(t) -> "~" ^ show_term t

(* List options: empty, single item, list *)
and show_term_list = function
    [] -> ""
  | [x] -> show_term x
  | (x::xs) -> show_term x ^ ", " ^ show_term_list xs

and show_let_bind = function
    New(name, letb) -> "new" ^ name ^ ";\n" ^ show_let_bind letb
  | Let(var, t, letb) -> "let " ^ var ^ " = " ^ show_term t ^ " in\n" ^ show_let_bind letb
  | LetEnd -> ""

and show_global_type = function
  Send(p, q, opt, t, g) -> p ^ " -> " (* TODO FIX *) ^ q ^ ": " ^ show_term t ^ "\n" ^ show_global_type g
| Branch(p, q, opt, t, branches) ->
  p ^ " -> " (* TODO FIX *) ^ q ^ ": match " ^ show_term t ^ " with {\n" ^ show_branches branches ^ "}\n"
| Compute(p, letb, g) ->
  p ^ " {\n" ^ show_let_bind letb ^ "}\n" ^ show_global_type g
| DefGlobal(name, params, g, g') ->
  name ^ "("^show_id_list params^")" ^ show_global_type g ^ "\nin\n"^show_global_type g'
| CallGlobal(name, params) ->
  name ^ "(" ^ show_term_list params ^ ")"
| GlobalEnd -> "end\n"

and show_branches = function
  [] -> ""
| ((t, g)::branches) ->
  show_term t ^ ": " ^ show_global_type g ^ "\n" ^ show_branches branches

and show_id_list = function
  [] -> ""
| [x] -> x
| (x::xs) -> x ^ ", " ^ show_id_list xs
