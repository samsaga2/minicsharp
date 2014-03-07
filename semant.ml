module A = Ast
module E = Env
module S = Symbol
module T = Types

let rec check prog =
  check_prog E.base_venv E.base_tenv prog

and check_prog venv tenv prog =
  match prog with
  | [] -> (venv, tenv, T.Unit)
  | decl::tail ->
     let (venv',tenv',typ)=check_decl venv tenv decl in
     check_prog venv' tenv' tail

and check_decl venv tenv decl =
  match decl with
  | A.FunDec (name,typ,args,body,pos) ->
     check_fundec venv tenv name typ args body pos
  | A.VarDec (name,typ,init,pos) ->
     check_vardec venv tenv name typ init pos

and check_fundec venv tenv name ret_typ args body pos =
  let arg_types = List.map (fun (_, arg_typ) -> actual_type tenv arg_typ pos) args in
  let typ = actual_type tenv ret_typ pos in
  let fun_entry = E.FunEntry (arg_types, typ) in
  let venv' = S.put venv name fun_entry in
  (venv', tenv, T.Unit)

and check_vardec venv tenv name typ init pos =
  (* TODO *)
  (venv, tenv, T.Unit)

and actual_type tenv sym pos =
  match S.get tenv sym with
  | None ->
     error ("undefined type "^(S.name sym)) pos;
     T.Nil
  | Some(typ) ->
     match typ with
     | T.Unit -> T.Unit
     | T.Nil -> T.Nil
     | T.Int -> T.Int

and error msg pos =
  let line = Lexer.line pos
  and col = Lexer.col pos in
  Printf.printf "%d:%d: %s" line col msg

(*
and assert_type t1 t2 pos =
  if t1<>t2 then
    error "types does not match" pos
*)
