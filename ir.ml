type inst =
  | Nop
  | Ret
  | Label of Symbol.symbol
  | Comment of string

let print_inst inst =
  match inst with
  | Nop ->
     "\tnop"
  | Ret ->
     "\tret"
  | Label sym ->
     (Symbol.name sym)^":"
  | Comment str ->
     "\t;; "^str

let print_code code =
  let insts = List.map print_inst code in
  String.concat "\n" insts
  
