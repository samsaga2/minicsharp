open Batteries

type reg = int

type inst =
  | Nop
  | RetUnit
  | Ret of reg
  | LoadArgInt of reg * int
  | LoadArgByte of reg * int
  | LoadConstInt of reg * int
  | LoadConstByte of reg * int
  | LoadInt of reg * Symbol.symbol
  | LoadByte of reg * Symbol.symbol
  | AddInt of reg * reg * reg
  | SubInt of reg * reg * reg
  | MulInt of reg * reg * reg
  | DivInt of reg * reg * reg
  | AddByte of reg * reg * reg
  | SubByte of reg * reg * reg
  | MulByte of reg * reg * reg
  | DivByte of reg * reg * reg
  | StoreInt of Symbol.symbol * reg
  | StoreByte of Symbol.symbol * reg

let print_inst inst =
  match inst with
  | Nop ->
     "\tnop"
  | RetUnit ->
     "\tret"
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
  | AddInt (dst,src1,src2) ->
     Printf.sprintf "\t%%%d = add.i %%%d,%%%d" dst src1 src2
  | SubInt (dst,src1,src2) ->
     Printf.sprintf "\t%%%d = sub.i %%%d,%%%d" dst src1 src2
  | MulInt (dst,src1,src2) ->
     Printf.sprintf "\t%%%d = mul.i %%%d,%%%d" dst src1 src2
  | DivInt (dst,src1,src2) ->
     Printf.sprintf "\t%%%d = div.i %%%d,%%%d" dst src1 src2
  | AddByte (dst,src1,src2) ->
     Printf.sprintf "\t%%%d = add.b %%%d,%%%d" dst src1 src2
  | SubByte (dst,src1,src2) ->
     Printf.sprintf "\t%%%d = sub.b %%%d,%%%d" dst src1 src2
  | MulByte (dst,src1,src2) ->
     Printf.sprintf "\t%%%d = mul.b %%%d,%%%d" dst src1 src2
  | DivByte (dst,src1,src2) ->
     Printf.sprintf "\t%%%d = div.b %%%d,%%%d" dst src1 src2
  | StoreInt (label,src) ->
     let label = Symbol.name label in
     Printf.sprintf "\tstore.i %s,%%%d" label src
  | StoreByte (label,src) ->
     let label = Symbol.name label in
     Printf.sprintf "\tstore.b %s,%%%d" label src

let print_insts insts =
  let insts = List.map print_inst insts in
  String.concat "\n" insts
