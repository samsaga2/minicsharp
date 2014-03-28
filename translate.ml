module I = Ir

type access =
  | InLabel of Symbol.symbol
  | InReg of I.reg

(* gen *)
let gen_int frame num =
  let dst = Frame.alloc_reg frame in
  (dst, [I.LoadConstInt (dst,num)])

let gen_byte frame num =
  let dst = Frame.alloc_reg frame in
  (dst, [I.LoadConstByte (dst,num)])

let gen_nil frame =
  gen_byte frame 0

let gen_funcarg frame typ index =
  let dst = Frame.alloc_reg frame in
  let inst = match typ with
    | Types.Int ->
       [I.LoadArgInt (dst,index)]
    | Types.Byte ->
       [I.LoadArgByte (dst,index)]
    | _ ->
       failwith "Internal error" in
  (dst, inst)

let gen_load_label frame label typ =
  let dst = Frame.alloc_reg frame in
  let inst = match typ with
    | Types.Int ->
       [I.LoadInt (dst,label)]
    | Types.Byte ->
       [I.LoadByte (dst,label)]
    | _ ->
       failwith "Internal error" in
  (dst, inst)

let gen_load_access frame typ access =
  match access with
  | InLabel (label) ->
     gen_load_label frame label typ
  | InReg (reg) ->
     (reg, [])

let gen_retunit () =
  [I.RetUnit]

let gen_ret src =
  [I.Ret src]

let gen_op_int frame op src1 src2 =
  let dst = Frame.alloc_reg frame in
  let insts = match op with
    | Ast.AddOp -> [I.AddInt (dst,src1,src2)]
    | Ast.SubOp -> [I.SubInt (dst,src1,src2)]
    | Ast.MulOp -> [I.MulInt (dst,src1,src2)]
    | Ast.DivOp -> [I.DivInt (dst,src1,src2)]
  in
  (dst, insts)

let gen_op_byte frame op src1 src2 =
  let dst = Frame.alloc_reg frame in
  let insts = match op with
    | Ast.AddOp -> [I.AddByte (dst,src1,src2)]
    | Ast.SubOp -> [I.SubByte (dst,src1,src2)]
    | Ast.MulOp -> [I.MulByte (dst,src1,src2)]
    | Ast.DivOp -> [I.DivByte (dst,src1,src2)]
  in
  (dst, insts)

let gen_op frame op typ src1 src2 =
  match typ with
  | Types.Int  -> gen_op_int frame op src1 src2
  | Types.Byte -> gen_op_byte frame op src1 src2
  | _ ->
     failwith "Internal error"

let gen_store typ label src =
  match typ with
  | Types.Int  -> [I.StoreInt (label,src)]
  | Types.Byte -> [I.StoreByte (label,src)]
  | _ ->
     failwith "Internal error"

let gen_callparam frame index reg typ =
  match typ with
  | Types.Int  -> [I.CallParamInt (reg,index)]
  | Types.Byte -> [I.CallParamByte (reg,index)]
  | _ ->
     failwith "Internal error"

let gen_call frame typ label =
  match typ with
  | Types.Unit ->
     (0, [I.CallUnit label])
  | _ ->
     let dst = Frame.alloc_reg frame in
     match typ with
     | Types.Int ->
        (dst, [I.CallInt (dst,label)])
     | Types.Byte ->
        (dst, [I.CallByte (dst,label)])
     | _ ->
        failwith "Internal error"

