
type principal = string
type ident = string

type term =
    Var of ident
  | Func of ident * term list
  | Eq of term * term
  | And of term * term
  | Or of term * term
  | Not of term

type let_bind =
    New of ident * let_bind
  | Let of ident * term * let_bind
  | LetEnd

type channel_options = { authentic: bool; secret: bool }

type global_type =
    Send of principal * principal * channel_options * term * global_type
  | Branch of principal * principal * term * (term * global_type) list
  | Compute of principal * let_bind * global_type
  | DefGlobal of ident * ident list * global_type * global_type
  | CallGlobal of ident * term list
  | GlobalEnd
