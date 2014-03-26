type frag = Ir.inst list

let frags = ref []

let add_frag (f:frag) =
  frags := f::!frags

let print_frags () =
  List.iter
    (fun (insts:frag) -> 
     print_endline (Ir.print_insts insts))
    !frags
