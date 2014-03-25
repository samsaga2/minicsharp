module A = Ast
module I = Ir

type context = {code:I.inst list}

let rec trans prog =
  let ctx = {code=[]} in
  let ctx' = trans_prog ctx prog in
  ctx'.code

and trans_prog ctx prog =
  match prog with
  | [] ->
     ctx
  | decl::tail ->
     trans_decl ctx decl

and trans_decl ctx decl =
  match decl with
  | A.FunDec (name,typ,args,body,pos) ->
     trans_fundec ctx name typ args body pos
  | A.VarDec (name,typ,init,pos) ->
     trans_vardec ctx name typ init pos

and trans_fundec ctx name typ args body pos =
  (* TODO *)
  let new_code = [I.Nop] in
  {code=ctx.code@new_code}

and trans_vardec ctx name typ init pos =
  (* TODO *)
  let new_code = [I.Nop] in
  {code=ctx.code@new_code}
