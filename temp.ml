let label_counter = ref 0

let new_label () =
  incr label_counter;
  let id = Printf.sprintf "_l%d" !label_counter in
  Symbol.symbol id

let reg_counter = ref 0

let reset_reg_counter () =
  reg_counter := 0

let new_reg () =
  incr reg_counter;
  let id = Printf.sprintf "r%d" !reg_counter in
  Symbol.symbol id
