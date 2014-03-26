type reg = int

type inst =
  | Nop
  | Ret
  | Label of Symbol.symbol
  | Comment of string
  | LoadArg of reg * reg

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
  | LoadArg (dst,src) ->
     Printf.sprintf "\t%d = loadarg %d" dst src

let print_code code =
  let insts = List.map print_inst code in
  String.concat "\n" insts
