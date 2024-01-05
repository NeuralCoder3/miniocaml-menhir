open MenhirLib.Ast
open MenhirLib.ParserInterface

let expressions = [
  "42";
  "fun (x:int) -> x + 1";
  "fun (x:int) -> x x";
  "5 + 3 * 4";
  "5 * 3 + 4";
]

let maxlen =
  expressions
  |> List.map String.length
  |> List.fold_left max 0

let pad n s =
  Printf.sprintf "%-*s" n s

let () = 
  List.iter (fun t ->
    let ast = parse_exp t in
    Printf.printf "\"%s\" -> %s\n" (pad maxlen t) (show_ast ast)
  ) expressions

(* see ParserInterface for lex *)