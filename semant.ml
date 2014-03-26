module A = Ast
module E = Env
module S = Symbol
module T = Types

let rec check prog =
  check_prog E.base_venv E.base_tenv prog

and check_prog venv tenv prog =
  match prog with
  | [] ->
     (venv,tenv)
  | decl::tail ->
     let (venv',tenv') = check_decl venv tenv decl in
     check_prog venv' tenv' tail

and check_decl venv tenv decl =
  match decl with
  | A.FunDec (name,typ,params,body,pos) ->
     check_fundec venv tenv name typ params body pos
  | A.VarDec (name,typ,init,pos) ->
     check_vardec venv tenv name typ init pos

and check_fundec venv tenv name ret_typ params body pos =
  assert_unique venv name pos;

  (* get function params final types *)
  let actual_param_type (_,typ) =
    actual_type tenv typ pos in
  let param_types = List.map actual_param_type params in

  (* get function return final type *)
  let return = actual_type tenv ret_typ pos in

  (* make a new func entry to var environtment *)
  let func = {E.label=Label.named_label name;
              E.params=param_types;
              E.return=return} in
  let entry = E.FunEntry func in

  let venv' = S.put venv name entry in

  (* make a new entry for each function param to the var env *)
  let push_param_to_env venv (param_sym,param_typ) =
    let typ = actual_type tenv param_typ pos in
    let entry = E.VarEntry {E.typ=typ} in
    S.put venv param_sym entry in
  let venv'' = List.fold_left push_param_to_env venv' params in

  (* check function body with the params env *)
  ignore (check_stmt venv'' tenv func body);

  (venv',tenv)

and check_vardec venv tenv name typ init pos =
  assert_unique venv name pos;
  let typ = actual_type tenv typ pos in
  begin
    match init with
    | None ->
       ()
    | Some(init) ->
       let exp_type = check_exp venv tenv init pos in
       assert_type typ exp_type pos;
  end;
  let entry = E.VarEntry {E.typ=typ} in
  let venv' = S.put venv name entry in
  (venv',tenv)

and check_exp venv tenv exp pos =
  match exp with
  | A.NilExp (_) ->
     T.Nil
  | A.IntExp (_,_) ->
     T.Int
  | A.VarExp (sym,pos) ->
     check_varexp venv tenv sym pos
  | A.CallExp (sym,args,pos) ->
     check_callexp venv tenv sym args pos
  | A.OpExp (left,op,right,pos) ->
     check_opexp venv tenv left op right pos

and check_varexp venv tenv sym pos =
  match S.get venv sym with
  | None ->
     error ("undeclared variable: "^(Symbol.name sym)) pos;
     T.Unit
  | Some(E.FunEntry _) ->
     error ("variable expected: "^(Symbol.name sym)) pos;
     T.Unit
  | Some(E.VarEntry varentry) ->
     varentry.E.typ

and check_callexp venv tenv sym args pos =
  match S.get venv sym with
  | None ->
     error ("undeclared function: "^(Symbol.name sym)) pos;
     T.Unit
  | Some(E.VarEntry _) ->
     error ("function expected: "^(Symbol.name sym)) pos;
     T.Unit
  | Some(E.FunEntry funentry) ->
     List.iter2
       (fun exp decl_arg_typ ->
        let exp_typ = check_exp venv tenv exp pos in
        assert_type exp_typ decl_arg_typ pos)
       args funentry.E.params;
     funentry.E.return

and check_opexp venv tenv left op right pos =
  let left = check_exp venv tenv left pos
  and right = check_exp venv tenv right pos in
  assert_type left right pos;
  assert_number left pos;
  left

and check_stmt venv tenv func stmt =
  match stmt with
  | A.LetStmt (name,typ,exp,pos) ->
     check_vardec venv tenv name typ exp pos
  | A.ReturnStmt (exp,pos) ->
     let ret_typ = check_exp venv tenv exp pos in
     assert_type ret_typ func.E.return pos;
     (venv,tenv)
  | A.SeqStmt (stmts,pos) ->
     ignore (List.fold_left
               (fun (venv',tenv') stmt ->
                check_stmt venv' tenv' func stmt)
               (venv,tenv)
               stmts);
     (venv,tenv)
  | A.IfStmt (cond,then_body,pos) ->
     let cond_typ = check_exp venv tenv cond pos in
     assert_type cond_typ T.Bool pos;
     ignore (check_stmt venv tenv func then_body);
     (venv,tenv)
  | A.IfElseStmt (cond,then_body,else_body,pos) ->
     let cond_typ = check_exp venv tenv cond pos in
     assert_type cond_typ T.Bool pos;
     ignore (check_stmt venv tenv func then_body);
     ignore (check_stmt venv tenv func else_body);
     (venv,tenv)
  | A.IgnoreStmt (exp,pos) ->
     let exp_typ = check_exp venv tenv exp pos in
     ignore exp_typ;
     (venv,tenv)

and actual_type tenv sym pos =
  match S.get tenv sym with
  | None ->
     error ("undefined type "^(S.name sym)) pos;
     T.Nil
  | Some(typ) ->
     match typ with
     | T.Unit -> T.Unit
     | T.Nil  -> T.Nil
     | T.Int  -> T.Int
     | T.Bool -> T.Bool

and error msg pos =
  let line = Lexer.line pos
  and col = Lexer.col pos in
  Printf.printf "%d:%d: %s\n%!" line col msg

and assert_type t1 t2 pos =
  if t1<>t2 then
    error "types does not match" pos

and assert_unique env sym pos =
  match S.get env sym with
  | None ->
     ()
  | Some _ ->
     error ("already defined "^(S.name sym)) pos

and assert_number typ pos =
  match typ with
  | T.Int ->
     ()
  | T.Unit | T.Nil | T.Bool ->
     error "must be a number" pos
