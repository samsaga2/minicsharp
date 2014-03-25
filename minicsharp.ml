let print_syntax_error lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
  let lnum = pos.Lexing.pos_lnum
  and cnum = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
  Printf.fprintf stderr "%d:%d: syntax error\n%!" cnum lnum

let compile in_buffer = 
  let lexbuf = Lexing.from_channel in_buffer in
  try
    let prog = Parser.program Lexer.token lexbuf in
    let (venv,tenv) = Semant.check prog in
    let ir = Translate.trans venv tenv prog in
    ignore ir
  with
  | Lexer.LexingError ->
     print_syntax_error lexbuf
  | Parser.Error -> 
     print_syntax_error lexbuf
  
let _  =
  compile stdin
