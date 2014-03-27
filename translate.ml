module I = Ir

type level = {mutable num_regs:int}

let alloc_reg level =
  let reg = level.num_regs in
  level.num_regs <- level.num_regs + 1;
  reg

let gen_int level num =
  let dst = alloc_reg level in
  [I.LoadConstInt (dst,num)]

let gen_nil level =
  gen_int level 0
