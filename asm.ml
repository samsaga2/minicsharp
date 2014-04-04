open Batteries

type oper = {opcode: string;
             dst: Ir.reg option;
             src1: Ir.reg option;
	     src2: Ir.reg option;
             label: Symbol.symbol option;
             num: int option}

type instr =
  | Oper of oper
  | Label of Symbol.symbol
  | Move of oper

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
     let int_opt_to_string n = Option.map_default string_of_int "" n
     and reg_opt_to_string n = Option.map_default (fun n -> "%"^(string_of_int n)) "" n
     and str_opt_to_string n = Option.map_default Symbol.name "" n in
     let dst	= reg_opt_to_string dst
     and src1	= reg_opt_to_string src1
     and src2	= reg_opt_to_string src2
     and label	= str_opt_to_string label
     and num	= int_opt_to_string num in
     let replaces = ["DST", dst;
		     "SRC1", src1; "SRC2", src2;
		     "LABEL", label; "NUM", num] in
     List.fold_left
       (fun str (sfrom,sto) ->
        String.nreplace str sfrom sto)
       opcode replaces
  | _ ->
     failwith "internal error"

let print_insts insts =
  let insts = List.map print_inst insts in
  String.concat "\n" insts
