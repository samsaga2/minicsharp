module S = Symbol

let rec translate prog =
  translate_prog Env.base_venv Env.base_tenv prog

and translate_prog venv tenv prog =
  match prog with
  | [] -> (venv, tenv, Types.Unit)
  | decl::tail ->
     let (venv',tenv',typ)=translate_decl venv tenv decl in
     translate_prog venv' tenv' tail

and translate_decl venv tenv decl =
  match decl with
  | Ast.FunDec (name,typ,args,body,pos) ->
     translate_fundec venv tenv name typ args body pos
  | Ast.VarDec (name,typ,init,pos) ->
     translate_vardec venv tenv name typ init pos

and translate_fundec venv tenv name typ args body pos =
  (* TODO *)
  (venv, tenv, Types.Unit)

and translate_vardec venv tenv name typ init pos =
  (* TODO *)
  (venv, tenv, Types.Unit)

(*
and assert_type t1 t2 pos =
  if t1<>t2 then
    error "types does not match" pos

and error msg pos =
  let line = Lexer.line pos
  and col = Lexer.col pos in
  Printf.printf "%d:%d: %s" line col msg
*)
