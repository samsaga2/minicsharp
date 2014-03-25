type inst =
  | Nop
  | Ret

let print_inst inst =
  match inst with
  | Nop ->
     "\tnop"
  | Ret ->
     "\tret"

let print_code code =
  let insts = List.map print_inst code in
  String.concat "\n" insts
  
