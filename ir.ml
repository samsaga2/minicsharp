type reg = int

type inst =
  | Nop
  | Ret
  | Label of Symbol.symbol
  | LoadArgInt of reg * reg

let print_inst inst =
  match inst with
  | Nop ->
     "\tnop"
  | Ret ->
     "\tret"
  | Label sym ->
     (Symbol.name sym)^":"
  | LoadArgInt (dst,src) ->
     Printf.sprintf "\t%%%d = loadarg.i %d" dst src

let print_insts insts =
  let insts = List.map print_inst insts in
  String.concat "\n" insts
