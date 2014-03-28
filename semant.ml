module A = Ast
module E = Env
module S = Symbol
module T = Types
module Tr = Translate

let rec check prog =
  ignore (check_prog E.base_venv E.base_tenv prog)

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

  (* get function return final type *)
  let return = actual_type tenv ret_typ pos in

  (* get function params final types *)
  let params_types = List.map
                      (fun (_,typ) -> actual_type tenv typ pos)
                      params in

  (* make a new func entry to var environtment *)
  let label = Label.named_label name in
  let func = E.make_fun label params_types return in
  let venv' = S.put venv name func in

  (* make a new entry for each function param to the var env *)
  let frame = Frame.new_frame () in
  let (venv'',arg_insts) = check_funcargs venv' tenv frame params pos in

  (* check function body with the params env *)
  let (insts,_,_) = check_stmt venv'' tenv frame return body in
  Frag.add_proc label (arg_insts@insts);

  (venv',tenv)

and check_funcargs venv tenv frame params pos =
  let index = ref 0 and insts = ref [] in
  let push_param_to_env venv (param_sym,param_typ) =
    (* arg actual type *)
    let typ = actual_type tenv param_typ pos in
    (* load arg insts *)
    let arg_insts = Tr.gen_funcarg frame typ !index in
    insts := !insts@arg_insts;
    incr index;
    (* push arg to venv *)
    let access = Tr.alloc_local frame in
    let entry = E.make_var typ access in
    S.put venv param_sym entry in
  let venv' = List.fold_left push_param_to_env venv params in
  (venv',!insts)

and check_vardec venv tenv name typ init pos =
  assert_unique venv name pos;
  let frame = Frame.new_frame ()
  and typ = actual_type tenv typ pos in
  begin
    match init with
    | None ->
       ()
    | Some(init) ->
       let (insts,exp_type) = check_exp venv tenv frame init pos in
       assert_type typ exp_type pos;
       Frag.add_var name insts
  end;
  let access = Tr.label name in
  let entry = E.make_var typ access in
  let venv' = S.put venv name entry in
  (venv',tenv)

and check_exp venv tenv frame exp pos =
  match exp with
  | A.NilExp (_) ->
     let insts = Tr.gen_nil frame in
     (insts, T.Nil)
  | A.ByteExp (num,_) ->
     let insts = Tr.gen_byte frame num in
     (insts, T.Byte)
  | A.IntExp (num,_) ->
     let insts = Tr.gen_int frame num in
     (insts, T.Int)
  | A.VarExp (sym,pos) ->
     check_varexp venv tenv frame sym pos
  | A.CallExp (sym,args,pos) ->
     check_callexp venv tenv frame sym args pos
  | A.OpExp (left,op,right,pos) ->
     check_opexp venv tenv frame left op right pos

and check_varexp venv tenv frame sym pos =
  match S.get venv sym with
  | None ->
     Error.undeclared_variable sym pos;
     ([], T.Unit)
  | Some(E.FunEntry _) ->
     Error.variable_expected sym pos;
     ([], T.Unit)
  | Some(E.VarEntry var) ->
     let insts = Tr.gen_load_access frame var.E.typ var.E.access in
     (insts, var.E.typ)

and check_callexp venv tenv frame sym args pos =
  match S.get venv sym with
  | None ->
     Error.undeclared_function sym pos;
     ([], T.Unit)
  | Some(E.VarEntry _) ->
     Error.function_expected sym pos;
     ([], T.Unit)
  | Some(E.FunEntry funentry) ->
     List.iter2
       (fun exp decl_arg_typ ->
        let (insts,typ) = check_exp venv tenv frame exp pos in
        assert_type typ decl_arg_typ pos)
       args funentry.E.params;
     ([], funentry.E.return) (* TODO *)

and check_opexp venv tenv frame left op right pos =
  let (linsts,left) = check_exp venv tenv frame left pos
  and (rinsts,right) = check_exp venv tenv frame right pos in
  assert_type left right pos;
  assert_number left pos;
  (rinsts@linsts, left) (* TODO *)

and check_stmt venv tenv frame rettyp stmt =
  match stmt with
  | A.LetStmt (name,typ,exp,pos) ->
     check_letstmt venv tenv frame name typ exp pos
  | A.ReturnStmt (exp,pos) ->
     let (insts,return) = check_exp venv tenv frame exp pos in
     assert_type return rettyp pos;
     ([],venv,tenv) (* TODO *)
  | A.SeqStmt (stmts,pos) ->
     let (insts,_,_) = 
       List.fold_left
         (fun (prev_insts,venv,tenv) stmt ->
          let (insts,venv,tenv) = check_stmt venv tenv frame rettyp stmt in
          (prev_insts@insts,venv,tenv))
         ([],venv,tenv)
         stmts in
     (insts,venv,tenv)
  | A.IfStmt (cond,then_body,pos) ->
     let (insts,cond_typ) = check_exp venv tenv frame cond pos in
     assert_type cond_typ T.Bool pos;
     ignore (check_stmt venv tenv frame rettyp then_body);
     ([],venv,tenv) (* TODO *)
  | A.IfElseStmt (cond,then_body,else_body,pos) ->
     let (insts,cond_typ) = check_exp venv tenv frame cond pos in
     assert_type cond_typ T.Bool pos;
     ignore (check_stmt venv tenv frame rettyp then_body);
     ignore (check_stmt venv tenv frame rettyp else_body);
     ([],venv,tenv) (* TODO *)
  | A.IgnoreStmt (exp,pos) ->
     let (insts,exp_typ) = check_exp venv tenv frame exp pos in
     ignore exp_typ;
     (insts,venv,tenv)

and check_letstmt venv tenv frame name typ init pos =
  assert_unique venv name pos;
  let typ = actual_type tenv typ pos in
  begin
    match init with
    | None ->
       ()
    | Some(init) ->
       let (insts,exp_type) = check_exp venv tenv frame init pos in
       (* TODO *)
       assert_type typ exp_type pos;
  end;
  let access = Tr.alloc_local frame in
  let entry = E.VarEntry {E.typ=typ;access=access} in
  let venv' = S.put venv name entry in
  ([],venv',tenv) (* TODO *)

and actual_type tenv sym pos =
  match S.get tenv sym with
  | None ->
     Error.undefined_typ sym pos;
     T.Nil
  | Some(typ) ->
     match typ with
     | T.Unit -> T.Unit
     | T.Nil  -> T.Nil
     | T.Byte -> T.Byte
     | T.Int  -> T.Int
     | T.Bool -> T.Bool

and assert_type t1 t2 pos =
  if t1<>t2 then
    Error.assert_type pos

and assert_unique env sym pos =
  match S.get env sym with
  | None ->
     ()
  | Some _ ->
     Error.assert_unique sym pos

and assert_number typ pos =
  match typ with
  | T.Int
  | T.Byte ->
     ()
  | T.Unit
  | T.Nil
  | T.Bool ->
     Error.assert_number pos
