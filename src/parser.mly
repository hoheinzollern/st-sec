%{
  open Types
%}
%token <string> ID
%token <int> NUM
%token COMMA COLON SEMI
%token LEFT_PAR RIGHT_PAR LEFT_ANGLE RIGHT_ANGLE LEFT_BRACE RIGHT_BRACE
%token EQ AND OR NOT DIV
%token NEW LET IN END MATCH WITH DATA PROBLEM PRINCIPALS FUNCTIONS EQUATIONS PROTOCOL
%token AT ARROW AUTH CONF AUTHCONF
%token EOF

%start <Types.problem option> program
%%

program:
| PROBLEM; COLON; n = ID; SEMI;
  PRINCIPALS; COLON; p = separated_list(COMMA, ID); SEMI;
  FUNCTIONS; COLON; f = separated_list(COMMA, fundef); SEMI;
  EQUATIONS; COLON; e = separated_list(COMMA, eqdef); SEMI;
  PROTOCOL; COLON; g = global_type; EOF
{ Some { name = n; principals = p; functions = f; equations = e; protocol = g } };

fundef:
| f = ID; DIV; arity = NUM; LEFT_BRACE; DATA; RIGHT_BRACE { (f, (arity, true)) }
| f = ID; DIV; arity = NUM { (f, (arity, false)) }

eqdef:
| lhs = term; EQ; rhs = term { (lhs, rhs) }

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
| EQ; t = term
  { PMatch(t) }
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
| prin1 = ID; chan = channel_options; prin2 = ID; COLON; x = ID; EQ; t = term; gt = global_type
  { Send(prin1, prin2, chan, x, t, gt ) }
| prin1 = ID; chan = channel_options; prin2 = ID; COLON; MATCH; t1 = term; WITH; LEFT_BRACE; branches = branch_list; RIGHT_BRACE
  { Branch(prin1, prin2, chan, t1, branches) }
| prin = ID; LEFT_BRACE; lb = let_bind; RIGHT_BRACE; gt = global_type
  { Compute(prin, lb, gt) }
| LET; name = ID; LEFT_PAR; params = separated_list(COMMA, param); RIGHT_PAR; EQ; gt1 = global_type; IN; gt2 = global_type
  { DefGlobal(name, params, gt1, gt2) }
| name = ID; LEFT_PAR; args = term_list; RIGHT_PAR
  { CallGlobal(name, args) }
| END
  { GlobalEnd };

param:
| x = ID; AT; p = ID { (x, p) }

branch_list:
| { [] }
| p = pattern; COLON; gt = global_type; branches = branch_list
  { ((p, gt)::branches) };
