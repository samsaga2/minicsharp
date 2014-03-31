type label = Symbol.symbol

let label_counter = ref 0

let new_label () : label =
  incr label_counter;
  let id = Printf.sprintf "_%d" !label_counter in
  Symbol.symbol id

let named_label sym : label =
  let id = Symbol.name sym in
  Symbol.symbol id
