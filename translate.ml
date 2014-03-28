module I = Ir

type access =
  | InLabel of Symbol.symbol
  | InReg of I.reg

(* access *)
let label label =
  InLabel label

let alloc_local frame =
  let reg = Frame.alloc_reg frame in
  InReg reg

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

let gen_ret src =
  [I.Ret src]
