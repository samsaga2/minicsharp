module A = Ast
module E = Env
module S = Symbol
module T = Types

type context = {venv:E.ventry Symbol.table;
                tenv:Types.t Symbol.table;
                rettype:Types.t}

let rec check prog =
  let ctx = {venv=E.base_venv;tenv=E.base_tenv;rettype=T.Unit} in
  let ctx' = check_prog ctx prog in
  (ctx'.venv,ctx'.tenv)

and check_prog ctx prog =
  match prog with
  | [] -> ctx
  | decl::tail ->
     let (ctx',typ) = check_decl ctx decl in
     check_prog ctx' tail

and check_decl ctx decl =
  match decl with
  | A.FunDec (name,typ,args,body,pos) ->
     check_fundec ctx name typ args body pos
  | A.VarDec (name,typ,init,pos) ->
     check_vardec ctx name typ init pos

and check_fundec ctx name ret_typ args body pos =
  assert_unique ctx.venv name pos;

  (* get function args final types *)
  let actual_arg_type (_,arg_typ) =
    actual_type ctx.tenv arg_typ pos in
  let arg_types = List.map actual_arg_type args in

  (* get function return final type *)
  let rettype = actual_type ctx.tenv ret_typ pos in

  (* make a new func entry to var environtment *)
  let entry = E.FunEntry {E.label=Temp.new_label();
                          E.args=arg_types;
                          E.rettype=rettype} in

  let venv' = S.put ctx.venv name entry in
  let ctx' = {ctx with venv=venv'} in

  (* make a new entry for each function arg to the var env *)
  let push_arg_to_env venv (arg_sym,arg_typ) =
    let typ = actual_type ctx.tenv arg_typ pos in
    let entry = E.VarEntry {E.typ=typ} in
    S.put venv arg_sym entry in
  let venv'' = List.fold_left push_arg_to_env venv' args in
  let ctx'' = {ctx with venv=venv''; rettype=rettype} in

  (* check function body with the args env *)
  ignore (check_stmt ctx'' body);

  (ctx', T.Unit)

and check_vardec ctx name typ init pos =
  assert_unique ctx.venv name pos;
  let typ = actual_type ctx.tenv typ pos in
  begin
    match init with
    | None -> ()
    | Some(init) ->
       begin
         let exp_type = check_exp ctx init pos in
         assert_type typ exp_type pos;
       end
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
     begin
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
            args funentry.E.args;
          funentry.E.rettype
     end
  | A.OpExp (left,op,right,pos) ->
     let left = check_exp ctx left pos
     and right = check_exp ctx right pos in
     assert_type left right pos;
     assert_number left pos;
     left

and check_varexp ctx sym pos  =
  match S.get ctx.venv sym with
  | None ->
     error ("undeclared variable: "^(Symbol.name sym)) pos;
     T.Unit
  | Some(E.FunEntry _) ->
     error ("variable expected: "^(Symbol.name sym)) pos;
     T.Unit
  | Some(E.VarEntry varentry) ->
     varentry.E.typ

and check_stmt ctx stmt =
  match stmt with
  | A.DeclStmt (decl,pos) ->
     check_decl ctx decl
  | A.ReturnStmt (exp,pos) ->
     let ret_typ = check_exp ctx exp pos in
     assert_type ret_typ ctx.rettype pos;
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
     | T.Nil -> T.Nil
     | T.Int -> T.Int
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
