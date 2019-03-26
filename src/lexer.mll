{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

(* part 3 *)
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

(* part 4 *)
rule read =
  parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }

  | ','      { COMMA }
  | '('      { LEFT_PAR }
  | ')'      { RIGHT_PAR }
  | '<'      { LEFT_ANGLE }
  | '>'      { RIGHT_ANGLE }
  | '='      { EQ }
  | '&'      { AND }
  | '|'      { OR }
  | '~'      { NOT }

  | id       { let s = Lexing.lexeme lexbuf in ID(s) }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }
