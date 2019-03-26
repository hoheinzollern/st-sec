%{ open Types %}
%token <string> ID
%token COMMA
%token LEFT_PAR
%token RIGHT_PAR
%token LEFT_ANGLE
%token RIGHT_ANGLE
%token EQ
%token AND
%token OR
%token NOT
%token EOF

%start <Types.term option> maybe_term
%%

maybe_term:
| t = term; EOF { Some t }
| EOF { None }

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
