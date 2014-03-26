let reg_counter = ref 0

let reset_reg_counter () =
  reg_counter := 0

let new_reg () : Ir.reg =
  let reg = !reg_counter in
  incr reg_counter;
  reg
