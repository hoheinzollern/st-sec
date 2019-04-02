%{ open Types %}
%token <string> ID
%token <string> PRINCIPAL
%token COMMA DOT COLON SEMI
%token LEFT_PAR RIGHT_PAR LEFT_ANGLE RIGHT_ANGLE LEFT_BRACE RIGHT_BRACE
%token EQ AND OR NOT
%token NEW LET END
%token ARROW
%token EOF

%start <Types.term option> maybe_term
%%

maybe_term:
| t = term; EOF { Some t }
| EOF { None }

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

term_list:
| l = separated_list(COMMA, term)
  { l };

let_bind:
| NEW; name = ID; SEMI; letb = let_bind
  { New(name, letb) }
| LET; name = ID; EQ; t = term; SEMI; letb = let_bind
  { Let(name, t, letb) }
| END
  { LetEnd() };

(* *->* *)
(*
channel_options:
| END
  { LetEnd() };
*)

global_type:
(* P -> Q <M>.G
Send of principal * principal * channel_options * term * global_type*)
| prin1 = PRINCIPAL; chan = channel_options; prin2 = PRINCIPAL; LEFT_ANGLE; t = term; RIGHT_ANGLE; DOT; gt = global_type
  { Send(prin1, print2, chan, t, gt ) }
(* P -> Q match M with { l : G }
Branch of principal * principal * term * (term * global_type) list *)
| prin1 = PRINCIPAL; chan = channel_options; prin2 = PRINCIPAL; t1 = term; LEFT_BRACE; t2 = term; COLON; gt = global_type; RIGHT_BRACE
  { Branch(print1, prin2, chan, t1, t2, gt) }
(* P { l }.G
Compute of principal * let_bind * global_type*)
| prin = PRINCIPAL; LEFT_BRACE; lb = let_bind; RIGHT_BRACE; DOT; gt = global_type
  { Compute(prin, lb, gt) }
(* let X(x) = G1 in G2
DefGlobal of ident * ident list * global_type * global_type*)
| name = ID; LEFT_PAR; name_list = ident_list; RIGHT_PAR; EQ; gt1 = global_type; gt2 = global_type
  { DefGlobal(name, name_list, gt1, gt2) }
(* X(M)
CallGlobal of ident * term list*)
| name = ID; LEFT_PAR; args = term_list; RIGHT_PAR
  { CallGlobal(name, args) }
(* end *)
| END
  { GlobalEnd() };

ident_list:
| l = separated_list(COMMA, ID)
  { l };
