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
  [I.LoadConstInt (dst,num)]

let gen_byte frame num =
  let dst = Frame.alloc_reg frame in
  [I.LoadConstByte (dst,num)]

let gen_nil frame =
  gen_byte frame 0

let gen_funcarg frame typ index =
  let dst = Frame.alloc_reg frame in
  match typ with
  | Types.Int ->
     [I.LoadArgInt (dst,index)]
  | Types.Byte ->
     [I.LoadArgByte (dst,index)]
  | _ ->
     failwith "Internal error"
