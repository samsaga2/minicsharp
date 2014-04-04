open Batteries

type reg = int

type inst =
  | Nop
  | RetUnit
  | Ret of reg
  | LoadParamInt of reg * int
  | LoadParamByte of reg * int
  | LoadParamBool of reg * int
  | ConstInt of reg * int
  | ConstByte of reg * int
  | ConstBool of reg * bool
  | LoadInt of reg * Symbol.symbol
  | LoadByte of reg * Symbol.symbol
  | LoadBool of reg * Symbol.symbol
  | AddInt of reg * reg * reg
  | SubInt of reg * reg * reg
  | MulInt of reg * reg * reg
  | DivInt of reg * reg * reg
  | EqInt of reg * reg * reg
  | EqByte of reg * reg * reg
  | EqBool of reg * reg * reg
  | AddByte of reg * reg * reg
  | SubByte of reg * reg * reg
  | MulByte of reg * reg * reg
  | DivByte of reg * reg * reg
  | StoreInt of Symbol.symbol * reg
  | StoreByte of Symbol.symbol * reg
  | StoreBool of Symbol.symbol * reg
  | CallParamInt of reg * int
  | CallParamByte of reg * int
  | CallParamBool of reg * int
  | CallUnit of Symbol.symbol
  | CallInt of reg * Symbol.symbol
  | CallByte of reg * Symbol.symbol
  | CallBool of reg * Symbol.symbol
  | Label of Symbol.symbol
  | Jump of Symbol.symbol
  | JumpFalse of reg * Symbol.symbol

let print_inst = function
  | Nop ->
     "\tnop"
  | RetUnit ->
     "\tret"
  | Ret (src) ->
     Printf.sprintf "\tret %%%d" src
  | LoadParamInt (dst,src) ->
     Printf.sprintf "\t%%%d = loadparam.i %d" dst src
  | LoadParamByte (dst,src) ->
     Printf.sprintf "\t%%%d = loadparam.b %d" dst src
  | LoadParamBool (dst,src) ->
     Printf.sprintf "\t%%%d = loadparam.t %d" dst src
  | ConstInt (dst,num) ->
     Printf.sprintf "\t%%%d = const.i %d" dst num
  | ConstByte (dst,num) ->
     Printf.sprintf "\t%%%d = const.b %d" dst num
  | ConstBool (dst,b) ->
     let b = if b then "true" else "false" in
     Printf.sprintf "\t%%%d = const.t %s" dst b
  | LoadInt (dst,label) ->
     let label = Symbol.name label in
     Printf.sprintf "\t%%%d = load.i %s" dst label
  | LoadByte (dst,label) ->
     let label = Symbol.name label in
     Printf.sprintf "\t%%%d = load.b %s" dst label
  | LoadBool (dst,label) ->
     let label = Symbol.name label in
     Printf.sprintf "\t%%%d = load.t %s" dst label
  | AddInt (dst,src1,src2) ->
     Printf.sprintf "\t%%%d = add.i %%%d,%%%d" dst src1 src2
  | SubInt (dst,src1,src2) ->
     Printf.sprintf "\t%%%d = sub.i %%%d,%%%d" dst src1 src2
  | MulInt (dst,src1,src2) ->
     Printf.sprintf "\t%%%d = mul.i %%%d,%%%d" dst src1 src2
  | DivInt (dst,src1,src2) ->
     Printf.sprintf "\t%%%d = div.i %%%d,%%%d" dst src1 src2
  | EqInt (dst,src1,src2) ->
     Printf.sprintf "\t%%%d = eq.i %%%d,%%%d" dst src1 src2
  | EqByte (dst,src1,src2) ->
     Printf.sprintf "\t%%%d = eq.b %%%d,%%%d" dst src1 src2
  | EqBool (dst,src1,src2) ->
     Printf.sprintf "\t%%%d = eq.t %%%d,%%%d" dst src1 src2
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
  | StoreBool (label,src) ->
     let label = Symbol.name label in
     Printf.sprintf "\tstore.t %s,%%%d" label src
  | CallParamInt (src,index) ->
     Printf.sprintf "\tcallparam.i %%%d,%d" src index
  | CallParamByte (src,index) ->
     Printf.sprintf "\tcallparam.b %%%d,%d" src index
  | CallParamBool (src,index) ->
     Printf.sprintf "\tcallparam.t %%%d,%d" src index
  | CallUnit label ->
     let label = Symbol.name label in
     Printf.sprintf "\tcall %s" label
  | CallInt (dst,label) ->
     let label = Symbol.name label in
     Printf.sprintf "\t%%%d = call.i %s" dst label
  | CallByte (dst,label) ->
     let label = Symbol.name label in
     Printf.sprintf "\t%%%d = call.b %s" dst label
  | CallBool (dst,label) ->
     let label = Symbol.name label in
     Printf.sprintf "\t%%%d = call.t %s" dst label
  | Label label ->
     let label = Symbol.name label in
     label^":"
  | Jump label ->
     let label = Symbol.name label in
     Printf.sprintf "\tjump %s" label
  | JumpFalse (reg,label) ->
     let label = Symbol.name label in
     Printf.sprintf "\tjumpfalse %%%d,%s" reg label

let print_insts insts =
  let insts = List.map print_inst insts in
  String.concat "\n" insts
