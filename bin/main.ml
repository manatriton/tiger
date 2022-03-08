open Core
open Tiger

let () =
  let lexbuf = Lexing.from_channel In_channel.stdin in
  let prog = Parser.prog Lexer.read lexbuf in
  Out_channel.print_endline
    (Sexp.to_string_hum ~indent:2 (Ast.sexp_of_exp prog))
