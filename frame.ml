type frame = {mutable num_regs:int}

let new_frame () =
  {num_regs=0}

let alloc_reg frame : Ir.reg =
  let reg = frame.num_regs in
  frame.num_regs <- frame.num_regs + 1;
  reg
