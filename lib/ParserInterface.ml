open Lexing

let colnum pos =
  (pos.pos_cnum - pos.pos_bol) - 1

let pos_string pos =
  let l = string_of_int pos.pos_lnum
  and c = string_of_int ((colnum pos) + 1) in
  "line " ^ l ^ ", column " ^ c

let parse' f s =
  let lexbuf = Lexing.from_string s in
  try
    f Lexer.token lexbuf
  with Parser.Error ->
    raise (Failure ("Parse error at " ^ (pos_string lexbuf.lex_curr_p)))

let parse_exp s =
  parse' Parser.topexpression s

let lex s : Parser.token list =
  let lexbuf = Lexing.from_string s in
  let rec loop l =
    let t = Lexer.token lexbuf in
    match t with
    | EOF -> List.rev l
    | _ -> loop (t :: l)
  in
  loop []

(* test using
dune ocaml utop
open MenhirLib
open ParserInterface
open Parser
lex "" 
*)