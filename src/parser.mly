%{
  open Types
  let rec to_idents = function
    [] -> []
  | (Var x::xs) -> x::to_idents xs
%}
%token <string> ID
%token COMMA DOT COLON SEMI
%token LEFT_PAR RIGHT_PAR LEFT_ANGLE RIGHT_ANGLE LEFT_BRACE RIGHT_BRACE
%token EQ AND OR NOT
%token NEW LET IN END MATCH WITH
%token ARROW AUTH CONF AUTHCONF
%token EOF

%start <Types.global_type option> start
%%

start:
| g = global_type; EOF { Some g };

(* Choose? *)
term:
| name = ID; LEFT_PAR; args = term_list; RIGHT_PAR
  { Func(name, args) }
| name = ID
  { Var(name) }
| LEFT_ANGLE; args = term_list; RIGHT_ANGLE
  { Tuple(args) }
| t1 = term; EQ; t2 = term
  { Eq(t1, t2) }
| t1 = term; AND; t2 = term
  { And(t1, t2) }
| t1 = term; OR; t2 = term
  { Or(t1, t2) }
| NOT; t = term
  { Not(t) };
| LEFT_PAR; t = term; RIGHT_PAR
  { t }

term_list:
| l = separated_list(COMMA, term)
  { l };

let_bind:
| NEW; name = ID; SEMI; letb = let_bind
  { New(name, letb) }
| LET; name = ID; EQ; t = term; SEMI; letb = let_bind
  { Let(name, t, letb) }
| { LetEnd };

channel_options:
| ARROW { { authentic = false; secret = false } }
| AUTH { { authentic = true; secret = false } }
| CONF { { authentic = false; secret = true } }
| AUTHCONF { { authentic = true; secret = true } };

global_type:
(* P -> Q <M>.G
Send of principal * principal * channel_options * term * global_type*)
| prin1 = ID; chan = channel_options; prin2 = ID; COLON; t = term; DOT; gt = global_type
  { Send(prin1, prin2, chan, t, gt ) }
(* P -> Q match M with { l : G }
Branch of principal * principal * term * (term * global_type) list *)
| prin1 = ID; chan = channel_options; prin2 = ID; COLON; MATCH; t1 = term; WITH; LEFT_BRACE; branches = branch_list; RIGHT_BRACE
  { Branch(prin1, prin2, chan, t1, branches) }
(* P { l }.G
Compute of principal * let_bind * global_type*)
| prin = ID; LEFT_BRACE; lb = let_bind; RIGHT_BRACE; DOT; gt = global_type
  { Compute(prin, lb, gt) }
(* let X(x) = G1 in G2
DefGlobal of ident * ident list * global_type * global_type*)
| name = ID; LEFT_PAR; name_list = term_list; RIGHT_PAR; EQ; gt1 = global_type; gt2 = global_type
  { DefGlobal(name, to_idents name_list, gt1, gt2) }
(* X(M)
CallGlobal of ident * term list*)
| name = ID; LEFT_PAR; args = term_list; RIGHT_PAR
  { CallGlobal(name, args) }
(* end *)
| END
  { GlobalEnd };

branch_list:
| { [] }
| p = pattern; COLON; gt = global_type; branches = branch_list
  { ((p, gt)::branches) }
