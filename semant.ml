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
  assert_dup venv name pos;
  let arg_types = List.map
                    (fun (_, arg_typ) -> actual_type tenv arg_typ pos)
                    args in
  let typ = actual_type tenv ret_typ pos in
  let entry = E.FunEntry (arg_types, typ) in
  let venv' = S.put venv name entry in
  check_stmt venv' tenv body

and check_vardec venv tenv name typ init pos =
  assert_dup venv name pos;
  let typ = actual_type tenv typ pos in
  begin
    match init with
    | None -> ()
    | Some(init) ->
       begin
         let exp_type = check_exp venv tenv init pos in
         assert_type typ exp_type pos;
       end
  end;
  let entry = E.VarEntry typ in
  let venv' = S.put venv name entry in
  (venv', tenv, T.Unit)

and check_exp venv tenv exp pos =
  match exp with
  | A.NilExp (_) ->
     T.Nil
  | A.IntExp (_,_) ->
     T.Int
  | A.VarExp (sym,pos) ->
     begin
       match S.get venv sym with
       | None ->
          error ("Undeclared variable: "^(Symbol.name sym)) pos;
          T.Nil
       | Some(E.FunEntry _) ->
          error ("Variable expected: "^(Symbol.name sym)) pos;
          T.Nil
       | Some(E.VarEntry typ) ->
          typ
     end
    | A.CallExp (_,_,_) ->
     (* TODO *)
     T.Int
  | A.OpExp (_,_,_,_) ->
     (* TODO *)
     T.Int

and check_stmt venv tenv stmt =
  match stmt with
  | A.DeclStmt (decl,pos) ->
     check_decl venv tenv decl
  | A.ReturnStmt (exp,pos) ->
     (* TODO check exp type is = func return type *)
     ignore (check_exp venv tenv exp pos);
     (venv,tenv,T.Unit)
  | A.SeqStmt (stmts,pos) ->
     ignore (List.fold_left
               (fun (venv',tenv',typ) stmt -> check_stmt venv' tenv' stmt)
               (venv,tenv,T.Unit)
               stmts);
     (venv,tenv,T.Unit)
  | A.IfStmt (cond,then_body,pos) ->
     let cond_typ = check_exp venv tenv cond pos in
     assert_type cond_typ T.Int pos; (* TODO boolean *)
     ignore (check_stmt venv tenv then_body);
     (venv,tenv,T.Nil)
  | A.IfElseStmt (cond,then_body,else_body,pos) ->
     let cond_typ = check_exp venv tenv cond pos in
     assert_type cond_typ T.Int pos; (* TODO boolean *)
     ignore (check_stmt venv tenv then_body);
     ignore (check_stmt venv tenv else_body);
     (venv,tenv,T.Nil)

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

and assert_type t1 t2 pos =
  if t1<>t2 then
    error "types does not match" pos

and assert_dup env sym pos =
  match S.get env sym with
  | None -> ()
  | Some _ ->
     error ("Already defined "^(S.name sym)) pos
