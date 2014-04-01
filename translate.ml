module I = Ir
module T = Types
module S = Symbol

type access =
  | InLabel of S.symbol
  | InReg of I.reg

type frame = {mutable num_regs:int}

let new_frame () =
  {num_regs=0}

let alloc_reg frame : Ir.reg =
  let reg = frame.num_regs in
  frame.num_regs <- frame.num_regs + 1;
  reg

(* gen *)
let gen_int frame num =
  let dst = alloc_reg frame in
  (dst, [I.ConstInt (dst,num)])

let gen_byte frame num =
  let dst = alloc_reg frame in
  (dst, [I.ConstByte (dst,num)])

let gen_bool frame b =
  let dst = alloc_reg frame in
  (dst, [I.ConstBool (dst,b)])

let gen_nil frame =
  gen_byte frame 0

let gen_funcparam frame typ index =
  let dst = alloc_reg frame in
  let inst = match typ with
    | T.Int ->
       [I.LoadParamInt (dst,index)]
    | T.Byte ->
       [I.LoadParamByte (dst,index)]
    | T.Bool ->
       [I.LoadParamBool (dst,index)]
    | _ ->
       failwith "Internal error" in
  (dst, inst)

let gen_load_label frame label typ =
  let dst = alloc_reg frame in
  let inst = match typ with
    | T.Int ->
       [I.LoadInt (dst,label)]
    | T.Byte ->
       [I.LoadByte (dst,label)]
    | T.Bool ->
       [I.LoadBool (dst,label)]
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
  let dst = alloc_reg frame in
  let insts = match op with
    | Ast.AddOp -> [I.AddInt (dst,src1,src2)]
    | Ast.SubOp -> [I.SubInt (dst,src1,src2)]
    | Ast.MulOp -> [I.MulInt (dst,src1,src2)]
    | Ast.DivOp -> [I.DivInt (dst,src1,src2)]
    | Ast.EqOp  -> [I.EqInt  (dst,src1,src2)]
  in
  (dst, insts)

let gen_op_byte frame op src1 src2 =
  let dst = alloc_reg frame in
  let insts = match op with
    | Ast.AddOp -> [I.AddByte (dst,src1,src2)]
    | Ast.SubOp -> [I.SubByte (dst,src1,src2)]
    | Ast.MulOp -> [I.MulByte (dst,src1,src2)]
    | Ast.DivOp -> [I.DivByte (dst,src1,src2)]
    | Ast.EqOp  -> [I.EqByte  (dst,src1,src2)]
  in
  (dst, insts)

let gen_op frame op typ src1 src2 =
  match typ with
  | T.Int  -> gen_op_int frame op src1 src2
  | T.Byte -> gen_op_byte frame op src1 src2
  | _ ->
     failwith "Internal error"

let gen_store typ label src =
  match typ with
  | T.Int  -> [I.StoreInt  (label,src)]
  | T.Byte -> [I.StoreByte (label,src)]
  | T.Bool -> [I.StoreBool (label,src)]
  | _ ->
     failwith "Internal error"

let gen_callparam frame index reg typ =
  match typ with
  | T.Int  -> [I.CallParamInt  (reg,index)]
  | T.Byte -> [I.CallParamByte (reg,index)]
  | T.Bool -> [I.CallParamBool (reg,index)]
  | _ ->
     failwith "Internal error"

let gen_call frame typ label =
  match typ with
  | T.Unit ->
     (0, [I.CallUnit label])
  | _ ->
     let dst = alloc_reg frame in
     match typ with
     | T.Int  -> (dst, [I.CallInt (dst,label)])
     | T.Byte -> (dst, [I.CallByte (dst,label)])
     | T.Bool -> (dst, [I.CallBool (dst,label)])
     | _ ->
        failwith "Internal error"

let gen_ifthen frame condreg theninsts =
  let endlbl = Label.new_label () in
  [I.JumpFalse (condreg,endlbl)]
  @theninsts
  @[I.Label endlbl]

let gen_ifthenelse frame condreg theninsts elseinsts =
  let elselbl = Label.new_label ()
  and endlbl = Label.new_label () in
  [I.JumpFalse (condreg,elselbl)]
  @theninsts
  @[I.Jump endlbl]
  @elseinsts
  @[I.Label endlbl]
