{
  open Lexing
  open Parser
  exception LexingError

  let update_loc lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
                                  Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
                                  Lexing.pos_bol = pos.Lexing.pos_cnum;
                                }
}

let newline = ('\010' | '\013' | "\013\010")
let whitespace = [' ' '\t']
let alpha = ['a'-'z' 'A'-'Z' '_']
let digit = ['0'-'9']

rule token = parse
          | eof { EOF }
          | newline { update_loc lexbuf; token lexbuf }
          | whitespace+ { token lexbuf }
          | "+" { PLUS }
          | "-" { MINUS }
          | "*" { MUL }
          | "/" { DIV }
          | "," { COMMA }
          | ";" { SEMICOLON }
          | "(" { LPAREN }
          | ")" { RPAREN }
          | "{" { LBRACK }
          | "}" { RBRACK }
          | "=" { EQ }
          | "return" { RETURN }
          | alpha (alpha|digit|"_")* as v { ID v }
          | digit+ as i { INT (int_of_string i) }
          | _ { raise LexingError }
