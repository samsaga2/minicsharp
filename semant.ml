module A = Ast
module E = Env
module S = Symbol
module T = Types

type context = {venv:E.ventry Symbol.table;
                tenv:Types.t Symbol.table;
                return:Types.t}

let rec check prog =
  let ctx = {venv=E.base_venv;tenv=E.base_tenv;return=T.Unit} in
  let ctx' = check_prog ctx prog in
  (ctx'.venv,ctx'.tenv)

and check_prog ctx prog =
  match prog with
  | [] ->
     ctx
  | decl::tail ->
     let (ctx',typ) = check_decl ctx decl in
     check_prog ctx' tail

and check_decl ctx decl =
  match decl with
  | A.FunDec (name,typ,params,body,pos) ->
     check_fundec ctx name typ params body pos
  | A.VarDec (name,typ,init,pos) ->
     check_vardec ctx name typ init pos

and check_fundec ctx name ret_typ params body pos =
  assert_unique ctx.venv name pos;

  (* get function params final types *)
  let actual_param_type (_,typ) =
    actual_type ctx.tenv typ pos in
  let param_types = List.map actual_param_type params in

  (* get function return final type *)
  let return = actual_type ctx.tenv ret_typ pos in

  (* make a new func entry to var environtment *)
  let entry = E.FunEntry {E.label=Temp.new_label();
                          E.params=param_types;
                          E.return=return} in

  let venv' = S.put ctx.venv name entry in
  let ctx' = {ctx with venv=venv'} in

  (* make a new entry for each function param to the var env *)
  let push_param_to_env venv (param_sym,param_typ) =
    let typ = actual_type ctx.tenv param_typ pos in
    let entry = E.VarEntry {E.typ=typ} in
    S.put venv param_sym entry in
  let venv'' = List.fold_left push_param_to_env venv' params in
  let ctx'' = {ctx with venv=venv''; return=return} in

  (* check function body with the params env *)
  ignore (check_stmt ctx'' body);

  (ctx', T.Unit)

and check_vardec ctx name typ init pos =
  assert_unique ctx.venv name pos;
  let typ = actual_type ctx.tenv typ pos in
  begin
    match init with
    | None ->
       ()
    | Some(init) ->
       let exp_type = check_exp ctx init pos in
       assert_type typ exp_type pos;
  end;
  let entry = E.VarEntry {E.typ=typ} in
  let venv' = S.put ctx.venv name entry in
  let ctx' = {ctx with venv=venv'} in
  (ctx', T.Unit)

and check_exp ctx exp pos =
  match exp with
  | A.NilExp (_) ->
     T.Nil
  | A.IntExp (_,_) ->
     T.Int
  | A.VarExp (sym,pos) ->
     check_varexp ctx sym pos
  | A.CallExp (sym,args,pos) ->
     check_callexp ctx sym args pos
  | A.OpExp (left,op,right,pos) ->
     check_opexp ctx left op right pos

and check_varexp ctx sym pos =
  match S.get ctx.venv sym with
  | None ->
     error ("undeclared variable: "^(Symbol.name sym)) pos;
     T.Unit
  | Some(E.FunEntry _) ->
     error ("variable expected: "^(Symbol.name sym)) pos;
     T.Unit
  | Some(E.VarEntry varentry) ->
     varentry.E.typ

and check_callexp ctx sym args pos =
  match S.get ctx.venv sym with
  | None ->
     error ("undeclared function: "^(Symbol.name sym)) pos;
     T.Unit
  | Some(E.VarEntry _) ->
     error ("function expected: "^(Symbol.name sym)) pos;
     T.Unit
  | Some(E.FunEntry funentry) ->
     List.iter2
       (fun exp decl_arg_typ ->
        let exp_typ = check_exp ctx exp pos in
        assert_type exp_typ decl_arg_typ pos)
       args funentry.E.params;
     funentry.E.return

and check_opexp ctx left op right pos =
  let left = check_exp ctx left pos
  and right = check_exp ctx right pos in
  assert_type left right pos;
  assert_number left pos;
  left

and check_stmt ctx stmt =
  match stmt with
  | A.DeclStmt (decl,pos) ->
     check_decl ctx decl
  | A.ReturnStmt (exp,pos) ->
     let ret_typ = check_exp ctx exp pos in
     assert_type ret_typ ctx.return pos;
     (ctx,T.Unit)
  | A.SeqStmt (stmts,pos) ->
     ignore (List.fold_left
               (fun (ctx',typ) stmt -> check_stmt ctx' stmt)
               (ctx,T.Unit)
               stmts);
     (ctx,T.Unit)
  | A.IfStmt (cond,then_body,pos) ->
     let cond_typ = check_exp ctx cond pos in
     assert_type cond_typ T.Bool pos;
     ignore (check_stmt ctx then_body);
     (ctx,T.Unit)
  | A.IfElseStmt (cond,then_body,else_body,pos) ->
     let cond_typ = check_exp ctx cond pos in
     assert_type cond_typ T.Bool pos;
     ignore (check_stmt ctx then_body);
     ignore (check_stmt ctx else_body);
     (ctx,T.Unit)
  | A.IgnoreStmt (exp,pos) ->
     let exp_typ = check_exp ctx exp pos in
     ignore exp_typ;
     (ctx,T.Unit)

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
