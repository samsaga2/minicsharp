let _  =
  let in_buffer = stdin in
  let lexbuf = Lexing.from_channel in_buffer in
  try
    let prog = Parser.program Lexer.token lexbuf in
    ignore (Semant.check prog)
  with
  | Parser.Error -> 
     let pos = Lexing.lexeme_start_p lexbuf in
     let lnum = pos.Lexing.pos_lnum
     and cnum = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
     Printf.fprintf stderr "%d:%d: syntax error\n%!" cnum lnum
