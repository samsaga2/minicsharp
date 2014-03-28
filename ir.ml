type reg = int

type inst =
  | Nop
  | Ret
  | LoadArgInt of reg * int
  | LoadArgByte of reg * int
  | LoadConstInt of reg * int
  | LoadConstByte of reg * int

let print_inst inst =
  match inst with
  | Nop ->
     "\tnop"
  | Ret ->
     "\tret"
  | LoadArgInt (dst,src) ->
     Printf.sprintf "\t%%%d = loadarg.i %d" dst src
  | LoadArgByte (dst,src) ->
     Printf.sprintf "\t%%%d = loadarg.b %d" dst src
  | LoadConstInt (dst,num) ->
     Printf.sprintf "\t%%%d = loadconst.i %d" dst num
  | LoadConstByte (dst,num) ->
     Printf.sprintf "\t%%%d = loadconst.b %d" dst num

let print_insts insts =
  let insts = List.map print_inst insts in
  String.concat "\n" insts
