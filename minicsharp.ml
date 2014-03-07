let _  =
  let lexbuf = Lexing.from_channel stdin in
  let prog = Parser.program Lexer.token lexbuf in
  let ir = Semant.check prog in
  ignore ir
