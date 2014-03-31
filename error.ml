let error msg pos =
  let line = Lexer.line pos
  and col = Lexer.col pos in
  Printf.printf "%d:%d: %s\n%!" line col msg

let undeclared_variable sym pos =
  error ("undeclared variable: "^(Symbol.name sym)) pos

let undeclared_function sym pos =
  error ("undeclared function: "^(Symbol.name sym)) pos

let variable_expected sym pos =
  error ("variable expected: "^(Symbol.name sym)) pos

let function_expected sym pos =
  error ("function expected: "^(Symbol.name sym)) pos

let undefined_typ sym pos =
  error ("undefined type "^(Symbol.name sym)) pos

let assert_type pos =
  error "types does not match" pos

let assert_unique sym pos =
  error ("already defined "^(Symbol.name sym)) pos

let assert_number pos =
  error "must be a number" pos
