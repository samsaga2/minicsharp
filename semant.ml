module A = Ast
module E = Env
module S = Symbol
module T = Types

type context = {venv:E.ventry Symbol.table;
                tenv:Types.t Symbol.table;
                rettype:Types.t}

let rec check prog =
  let ctx = {venv=E.base_venv;tenv=E.base_tenv;rettype=T.Unit} in
  check_prog ctx prog

and check_prog ctx prog =
  match prog with
  | [] -> ()
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
  let arg_types = List.map
                    (fun (_,arg_typ) -> actual_type ctx.tenv arg_typ pos)
                    args in
  let ret_typ = actual_type ctx.tenv ret_typ pos in
  let entry = E.FunEntry (arg_types, ret_typ) in
  let venv' = List.fold_left
                (fun venv (arg_sym,arg_typ) ->
                 let typ = actual_type ctx.tenv arg_typ pos in
                 let entry = E.VarEntry typ in
                 S.put venv arg_sym entry)
                (S.put ctx.venv name entry)
                args in
  let ctx' = {ctx with venv=venv';rettype=ret_typ} in
  check_stmt ctx' body

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
  let entry = E.VarEntry typ in
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
     begin
       match S.get ctx.venv sym with
       | None ->
          error ("undeclared variable: "^(Symbol.name sym)) pos;
          T.Nil
       | Some(E.FunEntry _) ->
          error ("variable expected: "^(Symbol.name sym)) pos;
          T.Nil
       | Some(E.VarEntry typ) ->
          typ
     end
    | A.CallExp (_,_,_) ->
     (* TODO *)
     T.Int
  | A.OpExp (left,op,right,pos) ->
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
