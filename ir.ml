open Batteries

type reg = int

type inst =
  | Nop
  | Ret of reg
  | LoadArgInt of reg * int
  | LoadArgByte of reg * int
  | LoadConstInt of reg * int
  | LoadConstByte of reg * int
  | LoadInt of reg * Symbol.symbol
  | LoadByte of reg * Symbol.symbol

let print_inst inst =
  match inst with
  | Nop ->
     "\tnop"
  | Ret (src) ->
     Printf.sprintf "\tret %%%d" src
  | LoadArgInt (dst,src) ->
     Printf.sprintf "\t%%%d = loadarg.i %d" dst src
  | LoadArgByte (dst,src) ->
     Printf.sprintf "\t%%%d = loadarg.b %d" dst src
  | LoadConstInt (dst,num) ->
     Printf.sprintf "\t%%%d = loadconst.i %d" dst num
  | LoadConstByte (dst,num) ->
     Printf.sprintf "\t%%%d = loadconst.b %d" dst num
  | LoadInt (dst,label) ->
     let label = Symbol.name label in
     Printf.sprintf "\t%%%d = load.i %s" dst label
  | LoadByte (dst,label) ->
     let label = Symbol.name label in
     Printf.sprintf "\t%%%d = load.b %s" dst label

let print_insts insts =
  let insts = List.map print_inst insts in
  String.concat "\n" insts
