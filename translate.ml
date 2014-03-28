module I = Ir

type access =
  | InLabel of Symbol.symbol
  | InReg of I.reg

let label label =
  InLabel label

let alloc_local frame =
  let reg = Frame.alloc_reg frame in
  InReg reg

let gen_int frame num =
  let dst = Frame.alloc_reg frame in
  [I.LoadConstInt (dst,num)]

let gen_nil frame =
  gen_int frame 0
