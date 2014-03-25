type inst =
  | Nop

let print_inst inst =
  match inst with
  | Nop ->
    "\tnop"

let print_code code =
  let insts = List.map print_inst code in
  String.concat "\n" insts
  
