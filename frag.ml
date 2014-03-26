type frag = 
  | Var of Label.label * Ir.inst list
  | Proc of Ir.inst list

let frags = ref []

let add_frag (f:frag) =
  frags := f::!frags

let add_var name insts =
  let frag = Var (name,insts) in
  add_frag frag

let add_proc insts =
  let frag = Proc insts in
  add_frag frag

(* pretty print *)

let print_frag = function
  | Var (label, insts) ->
     let comm = ("\t;; initialization of global var "^(Symbol.name label)) in
     print_endline comm;
     print_endline (Ir.print_insts insts)
  | Proc insts ->
     print_endline (Ir.print_insts insts)

let print_insts insts =
  print_endline (Ir.print_insts insts)

let print_var_frags () =
  print_endline "init:";
  List.iter (function
              | Var (label, insts) ->
                 print_insts insts;
                 print_endline ("\t;; store to "^(Symbol.name label))
              | _ ->
                 ())
            !frags;
  print_endline "\tret"

let print_proc_frags () = 
  List.iter (function
              | Proc (insts) ->
                 print_insts insts
              | _ ->
                 ())
            !frags

let print_frags () =
  print_var_frags ();
  print_proc_frags ()
