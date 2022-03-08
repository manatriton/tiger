{
  open Ast
  open Lexing

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- {
      pos with pos_bol = pos.pos_cnum;
               pos_lnum = pos.pos_lnum + 1
    }

  exception SyntaxError of string
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let int = ['0'-'9']+

rule read =
  parse
    | '+'         { PLUS }
    | '-'         { MINUS }
    | '*'         { TIMES }
    | '/'         { DIVIDE }
    | '&'         { AND }
    | '|'         { OR }
    | "<="        { LE }
    | '<'         { LT }
    | ">="        { GE }
    | '>'         { GT }
    | '.'         { DOT }
    | '}'         { RBRACE }
    | '{'         { LBRACE }
    | ']'         { RBRACK }
    | '['         { LBRACK }
    | ')'         { RPAREN }
    | '('         { LPAREN }
    | ';'         { SEMICOLON }
    | ':'         { COLON }
    | ','         { COMMA }
    | '='         { EQ }
    | "<>"        { NEQ }
    | ":="        { ASSIGN }
    | "type"      { TYPE }
    | "var"       { VAR }
    | "function"  { FUNCTION }
    | "break"     { BREAK }
    | "of"        { OF }
    | "end"       { END }
    | "in"        { IN }
    | "nil"       { NIL }
    | "let"       { LET }
    | "do"        { DO }
    | "to"        { TO }
    | "for"       { FOR }
    | "while"     { WHILE }
    | "else"      { ELSE }
    | "then"      { THEN }
    | "if"        { IF }
    | "array"     { ARRAY }
    | '"'         { read_string (Buffer.create 17) lexbuf }
    | "/*"        { read_comment 1 lexbuf }
    | int         { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | id          { ID (Lexing.lexeme lexbuf) }
    | white       { read lexbuf }
    | newline     { read lexbuf }
    | _           { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
    | eof         { EOF }

and read_string buf =
  parse
    | '"'         { STRING (Buffer.contents buf) }
    | [^ '"']+
                  { 
                    Buffer.add_string buf (Lexing.lexeme lexbuf);
                    read_string buf lexbuf
                  }
    | _           { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
    | eof         { raise (SyntaxError ("String is not terminated")) }

and read_comment depth =
  parse
    | "/*"        { read_comment (depth + 1) lexbuf }
    | "*/"        { if depth = 1 then read lexbuf else read_comment (depth - 1) lexbuf }
    | _           { read_comment depth lexbuf }
