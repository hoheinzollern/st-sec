open Lexer
open Lexing
open Printf

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.program Lexer.read lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let rec print_errors = function
  | (msg, g)::err ->
    fprintf stderr "%s in %s\n" msg (Types.show_global_type_nr g);
    print_errors err
  | [] -> ()

let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some g ->
    (* let errors = Typecheck.check g ["Alice", []; "TPM", []] [] ["enc", (2, false); "dec", (2, false)] in *)
    let errors = Typecheck.check g ["Alice", []; "Bob", []; "Charlie", []] [] ["enc", (2, true); "dec", (2, false)] in
    print_errors errors;
    printf "%s\n" (Types.show_global_type g);
    parse_and_print lexbuf
  | None -> ()

let loop filename () =
  let inx = open_in filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  close_in inx

let main = loop (Sys.argv.(1)) ()
