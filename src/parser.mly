%{
  open Types
  let rec to_idents = function
    [] -> []
  | (Var x::xs) -> x::to_idents xs
%}
%token <string> ID
%token COMMA COLON SEMI
%token LEFT_PAR RIGHT_PAR LEFT_ANGLE RIGHT_ANGLE LEFT_BRACE RIGHT_BRACE
%token EQ AND OR NOT
%token NEW LET IN END MATCH WITH
%token ARROW AUTH CONF AUTHCONF
%token EOF

%start <Types.global_type option> program
%%

program:
| g = global_type; EOF { Some g };

(* Choose? *)
term:
| name = ID
  { Var(name) }
| name = ID; LEFT_PAR; args = term_list; RIGHT_PAR
  { Func(name, args) }
| LEFT_ANGLE; args = term_list; RIGHT_ANGLE
  { Tuple(args) }
| t1 = term; EQ; t2 = term
  { Eq(t1, t2) }
| t1 = term; AND; t2 = term
  { And(t1, t2) }
| t1 = term; OR; t2 = term
  { Or(t1, t2) }
| NOT; t = term
  { Not(t) }
| LEFT_PAR; t = term; RIGHT_PAR
  { t };

term_list:
| l = separated_list(COMMA, term)
  { l };

pattern:
| name = ID
  { PVar(name) }
(* =term *)
| EQ; t = term
  { PMatch(t) }
(* match enc(x, k) with ... *)
| name = ID; LEFT_PAR; pargs = pattern_list; RIGHT_PAR
  { PFunc(name, pargs) }
| LEFT_ANGLE; pargs = pattern_list; RIGHT_ANGLE
  { PTuple(pargs) }

pattern_list:
| l = separated_list(COMMA, pattern)
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
| prin1 = ID; chan = channel_options; prin2 = ID; COLON; t = term; gt = global_type
  { Send(prin1, prin2, chan, t, gt ) }
| prin1 = ID; chan = channel_options; prin2 = ID; COLON; MATCH; t1 = term; WITH; LEFT_BRACE; branches = branch_list; RIGHT_BRACE
  { Branch(prin1, prin2, chan, t1, branches) }
| prin = ID; LEFT_BRACE; lb = let_bind; RIGHT_BRACE; gt = global_type
  { Compute(prin, lb, gt) }
| LET; name = ID; LEFT_PAR; name_list = term_list; RIGHT_PAR; EQ; gt1 = global_type; IN; gt2 = global_type
  { DefGlobal(name, to_idents name_list, gt1, gt2) }
| name = ID; LEFT_PAR; args = term_list; RIGHT_PAR
  { CallGlobal(name, args) }
| END
  { GlobalEnd };

branch_list:
| { [] }
| p = pattern; COLON; gt = global_type; branches = branch_list
  { ((p, gt)::branches) };
