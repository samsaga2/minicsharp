type frag = 
  | Var of Label.label * Ir.inst list
  | Proc of Label.label * Ir.inst list

let frags = ref []

let add_frag (f:frag) =
  frags := f::!frags

let add_var label insts =
  let frag = Var (label,insts) in
  add_frag frag

let add_proc label insts =
  let frag = Proc (label,insts) in
  add_frag frag

(* pretty print *)
let print_insts insts =
  print_endline (Ir.print_insts insts)

let print_frag = function
  | Var (label, insts) ->
     let comm = ("\t;; initialization of global var "^(Symbol.name label)) in
     print_endline comm;
     print_insts insts
  | Proc (label,insts) ->
     Printf.printf "%s:\n" (Symbol.name label);
     print_insts insts

let print_frags () =
  List.iter print_frag !frags
