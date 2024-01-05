(* we are strictly following the provided ocaml lexer and parser *)
{
open Lexing
open Parser

let advance_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  let pos' = { pos with
    pos_bol = lexbuf.lex_curr_pos;
    pos_lnum = pos.pos_lnum + 1
  } in
  lexbuf.lex_curr_p <- pos'
}

let digit = ['0'-'9']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let whitespace = [' ' '\t' '\r' '\n']

let number = digit+

let identifier = lowercase (lowercase | uppercase | digit | '_'  | '\'')*

(* Rules *)

(* note that keywords are not lexed greedily -- "letter" is correctly parsed as var *)
rule token = parse
  (* keywords *)
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "in" { IN }
  | "true" { BCON true }
  | "false" { BCON false }
  | "fun" { FUN }
  | "rec" { REC }
  | "int" { INT }
  | "bool" { BOOL }

  (* symbols *)
  | '(' { LP }
  | ')' { RP }
  | "->" { ARROW }
  | '=' { EQ }
  | '+' { ADD }
  | '-' { SUB }
  | '*' { MUL }
  | ':' { COLON }
  | "<=" { LEQ }

  (* combined tokens *)
  | number as n { ICON (int_of_string n) }
  | identifier as id { VAR id }

  (* etc. *)
  | whitespace { token lexbuf }
  | eof { EOF }
  | _ { raise (Failure ("Character not allowed in source text: '" ^ lexeme lexbuf ^ "'")) }
