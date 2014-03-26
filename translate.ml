module S = Symbol
module E = Env
module A = Ast
module I = Ir
module T = Types

type context = {venv:E.ventry S.table;
                tenv:T.t S.table;
                regs:I.reg S.table;
                code:I.inst list}

let rec trans venv tenv prog =
  let ctx = {venv=venv;
             tenv=tenv;
             regs=S.empty;
             code=[]} in
  let ctx' = trans_prog ctx prog in
  ctx'.code

and trans_prog ctx prog =
  match prog with
  | [] ->
     ctx
  | decl::tail ->
     let ctx' = trans_decl ctx decl in
     trans_prog ctx' tail

and trans_decl ctx decl =
  match decl with
  | A.FunDec (name,typ,args,body,pos) ->
     trans_fundec ctx name typ args body pos
  | A.VarDec (name,typ,init,pos) ->
     trans_vardec ctx name typ init pos

and trans_fundec ctx name typ args body pos =
  match S.get ctx.venv name with
  | Some(E.FunEntry func) ->
     (* function header code *)
     let header_code = [I.Label func.E.label] in

     (* function args code *)
     let arg_regs = List.map
                      (fun (arg_typ,arg_sym) ->
                       (arg_sym,Temp.new_reg ()))
                      args in
     let args_code = List.mapi
                       (fun idx (_,arg_reg) ->
                        Ir.LoadArgInt (arg_reg,idx))
                       arg_regs in
     let ctx' = {ctx with regs=E.extend_env ctx.regs arg_regs;
                          code=ctx.code@header_code@args_code} in

     (* function body code *)
     let ctx'' = trans_stmt ctx' body pos in

     (* function footer code *)
     let footer_code = [I.Ret] in
     {ctx with code=ctx''.code@footer_code}
  | _ ->
     failwith "internal error"

and trans_vardec ctx name typ init pos =
  (* TODO *)
  let new_code = [I.Nop] in
  {ctx with code=ctx.code@new_code}

and trans_stmt ctx body pos =
  (* TODO *)
  let code = [I.Nop] in
  {ctx with code=ctx.code@code}

