module S = Symbol
module E = Env
module A = Ast
module I = Ir
module T = Types

type context = {venv:E.ventry S.table;
                tenv:T.t S.table;
                nenv:S.symbol S.table;
                code:I.inst list}

let rec trans venv tenv prog =
  let ctx = {venv=venv;
             tenv=tenv;
             nenv=S.empty;
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
  let func_label = Temp.new_label () in
  let ctx' = {ctx with nenv=(S.put ctx.nenv name func_label)} in
  let new_code = [I.Comment (S.name name);
                  I.Label func_label;
                  I.Ret] in
  (* TODO *)
  {ctx' with code=ctx'.code@new_code}

and trans_vardec ctx name typ init pos =
  (* TODO *)
  let new_code = [I.Nop] in
  {ctx with code=ctx.code@new_code}
