open Z80machine
open Machine

let print_syntax_error = function
  | None ->
     Printf.fprintf stderr "syntax error\n%!"
  | Some(lexbuf) ->
     let pos = Lexing.lexeme_start_p lexbuf in
     let lnum = pos.Lexing.pos_lnum
     and cnum = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
     Printf.fprintf stderr "%d:%d: syntax error\n%!" cnum lnum

let compile in_buffer =
  try
    let lexbuf = Lexing.from_channel in_buffer in
    try
      let prog = Parser.program Lexer.token lexbuf in
      Semant.check prog;
      let asm = Z80Machine.asm () in
      ()
      (* print_endline (Asm.print_insts asm) *)
    with
    | Parser.Error ->
       print_syntax_error (Some lexbuf)
  with
  | Lexer.LexingError ->
     print_syntax_error None

let _  =
  compile stdin
