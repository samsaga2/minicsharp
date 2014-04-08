open Batteries

type t = {opcode: string;
          dst: Ir.reg option;
          src1: Ir.reg option;
	  src2: Ir.reg option;
          label: Symbol.t option;
          num: int option}

type instr =
  | Oper of t
  | Label of Symbol.t
  | Move of t

let gen_oper opcode ?dst ?src1 ?src2 ?label ?num () =
  Oper {opcode="\t"^opcode; dst=dst; src1=src1; src2=src2; label=label; num=num}

let gen_label label =
  Label label

let gen_move opcode dst src1 =
  Move {opcode=opcode; dst=dst; src1=src1; src2=None; label=None; num=None}

let rec print_inst = function
  | Label label ->
     (Symbol.name label)^":"
  | Oper oper
  | Move oper ->
     print_oper oper

and print_oper = function
  | {opcode=opcode; dst=dst; src1=src1; src2=src2; label=label; num=num} ->
     let string_from_opt_int n = Option.map_default string_of_int "" n
     and string_from_opt_reg n = Option.map_default (fun n -> "%"^(string_of_int n)) "" n
     and string_from_opt_str n = Option.map_default Symbol.name "" n in
     let dst	= string_from_opt_reg dst
     and src1	= string_from_opt_reg src1
     and src2	= string_from_opt_reg src2
     and label	= string_from_opt_str label
     and num	= string_from_opt_int num in
     let replaces = ["DST", dst;
		     "SRC1", src1; "SRC2", src2;
		     "LABEL", label; "NUM", num] in
     List.fold_left
       (fun str (sfrom,sto) ->
        String.nreplace str sfrom sto)
       opcode replaces

let print_insts insts =
  let insts = List.map print_inst insts in
  String.concat "\n" insts
