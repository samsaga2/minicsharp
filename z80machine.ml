open Machine

module Z80Machine : Machine = struct
  let global_label = Symbol.symbol "_init"

  let rec asm () =
    let init = asm_init () in
    let procs = asm_procs () in
    init@procs

  and asm_init () =
    let frag_vars = List.filter Frag.is_var !Frag.frags in
    let prologue = [Asm.gen_label global_label]
    and epilogue = [Asm.gen_oper "ret" ()]
    and body = List.flatten (List.map asm_var frag_vars) in
    prologue@body@epilogue

  and asm_var = function
    | Frag.Var (_,insts) -> asm_insts insts
    | _ -> failwith "internal error"

  and asm_procs () =
    let frag_procs = List.filter Frag.is_proc !Frag.frags in
    List.flatten (List.map asm_proc frag_procs)

  and asm_proc = function
    | Frag.Proc (name,insts) -> 
       let prologue = [Asm.gen_label (Label.named_label name)]
       and epilogue = [Asm.gen_oper "ret" ()]
       and body = asm_insts insts in
       prologue@body@epilogue
    | _ -> failwith "internal error"

  and asm_insts insts =
    let bblocks = Bblock.make insts in

(* TEST
    let bblocks = List.map Ir.print_insts bblocks in
    let bblocks = String.concat "\n;; BLOCKS\n" bblocks in
    print_endline "---";
    print_endline bblocks;
    print_endline "---";
*)

    (* TODO liveness *)
    (* TODO register allocator *)
    (* TODO asm_inst with allocated regs *)
    List.flatten (List.map asm_inst insts)

  and asm_inst = function
    | Ir.Nop ->
       [Asm.gen_oper "nop" ()]
    | Ir.RetUnit
    | Ir.Ret _ ->
       [Asm.gen_oper "ret" ()]
    | Ir.LoadParamInt (dst,index)
    | Ir.LoadParamByte (dst,index)
    | Ir.LoadParamBool (dst,index) ->
       []
    | Ir.ConstInt (reg,num)
    | Ir.ConstByte (reg,num)-> 
       [Asm.gen_oper "ld DST,NUM" ~dst:reg ~num:num ()]
    | Ir.ConstBool (reg,true) ->
       [Asm.gen_oper "ld DST,NUM" ~dst:reg ~num:255 ()]
    | Ir.ConstBool (reg,false) ->
       [Asm.gen_oper "ld DST,NUM" ~dst:reg ~num:0 ()]
    | Ir.LoadInt (dst,label)
    | Ir.LoadByte (dst,label)
    | Ir.LoadBool (dst,label) ->
       [Asm.gen_oper "ld DST,(LABEL)" ~dst:dst ~label:label ()]
    | Ir.AddInt (dst,src1,src2)
    | Ir.AddByte (dst,src1,src2) ->
       [Asm.gen_oper "add DST,SRC1" ~dst:dst ~src1:src2 ()]
    | Ir.SubInt (dst,src1,src2) ->
       [Asm.gen_oper "or a" ();
	Asm.gen_oper "sbc DST,SRC1" ~dst:dst ~src1:src2 ()]
    | Ir.SubByte (dst,src1,src2) ->
       [Asm.gen_oper "sub DST,SRC1" ~dst:dst ~src1:src2 ()]
    | Ir.MulInt (dst,src1,src2) ->
       [Asm.gen_oper "call math_mulint" ()]
    | Ir.MulByte (dst,src1,src2) ->
       [Asm.gen_oper "call math_mulbyte" ()]
    | Ir.DivInt (dst,src1,src2) ->
       [Asm.gen_oper "call math_divint" ()]
    | Ir.DivByte (dst,src1,src2) ->
       [Asm.gen_oper "call math_divbyte" ()]
    | Ir.EqInt (dst,src1,src2)
    | Ir.EqByte (dst,src1,src2)
    | Ir.EqBool (dst,src1,src2) ->
       [Asm.gen_oper "TODO eq DST SRC1 SRC2" ~dst:dst ~src1:src1 ~src2:src2 ()]
    | Ir.StoreInt (label,src)
    | Ir.StoreByte (label,src)
    | Ir.StoreBool (label,src) ->
       [Asm.gen_oper "ld (LABEL),SRC1" ~src1:src ~label:label ()]
    | Ir.CallParamInt _
    | Ir.CallParamByte _
    | Ir.CallParamBool _ ->
       []
    | Ir.CallUnit (label)
    | Ir.CallInt (_,label)
    | Ir.CallByte (_,label)
    | Ir.CallBool (_,label) ->
       [Asm.gen_oper "call LABEL" ~label:label ()]
    | Ir.Label (label) ->
       [Asm.gen_label label]
    | Ir.Jump (label) ->
       [Asm.gen_oper "jp LABEL" ~label:label ()]
    | Ir.JumpFalse (src,label) ->
       [Asm.gen_oper "or a" ();
	Asm.gen_oper "jr z,LABEL" ~label:label ()]
end
