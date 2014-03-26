let label_counter = ref 0

let new_label () =
  incr label_counter;
  let id = Printf.sprintf "_l%d" !label_counter in
  Symbol.symbol id

let reg_counter = ref 0

let reset_reg_counter () =
  reg_counter := 0

let new_reg () : Ir.reg =
  let reg = !reg_counter in
  incr reg_counter;
  reg
