open Batteries

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
  let frame = Tr.new_frame () in
  let (venv'',param_insts) = check_funcparams venv' tenv frame params pos in

  (* check function body with the params env *)
  let (insts,_,_) = check_stmt venv'' tenv frame return body in
  Frag.add_proc label (param_insts@insts);

  (venv',tenv)

and check_funcparams venv tenv frame params pos =
  let index = ref 0 and insts = ref [] in
  let push_param_to_env venv (param_sym,param_typ) =
    (* param actual type *)
    let typ = actual_type tenv param_typ pos in
    (* load param insts *)
    let (reg,param_insts) = Tr.gen_funcparam frame typ !index in
    insts := !insts@param_insts;
    incr index;
    (* push param to venv *)
    let access = Tr.InReg reg in
    let entry = E.make_var typ access in
    S.put venv param_sym entry in
  let venv' = List.fold_left push_param_to_env venv params in
  (venv',!insts)

and check_vardec venv tenv name typ init pos =
  assert_unique venv name pos;
  let frame = Tr.new_frame ()
  and typ = actual_type tenv typ pos
  and label = Label.named_label name in
  begin
    match init with
    | None ->
       ()
    | Some(init) ->
       let (reg,init_insts,exp_type) = check_exp venv tenv frame init pos in
       assert_type typ exp_type pos;
       let insts = init_insts@(Tr.gen_store typ label reg) in
       Frag.add_var name insts
  end;
  let access = Tr.InLabel label in
  let entry = E.make_var typ access in
  let venv' = S.put venv name entry in
  (venv',tenv)

and check_exp venv tenv frame exp pos =
  match exp with
  | A.NilExp (_) ->
     let (reg,insts) = Tr.gen_nil frame in
     (reg,insts,T.Nil)
  | A.ByteExp (num,_) ->
     let (reg,insts) = Tr.gen_byte frame num in
     (reg,insts,T.Byte)
  | A.IntExp (num,_) ->
     let (reg,insts) = Tr.gen_int frame num in
     (reg,insts,T.Int)
  | A.BoolExp (b,_) ->
     let (reg,insts) = Tr.gen_bool frame b in
     (reg,insts,T.Bool)
  | A.VarExp (sym,pos) ->
     check_varexp venv tenv frame sym pos
  | A.CallExp (sym,params,pos) ->
     check_callexp venv tenv frame sym params pos
  | A.OpExp (left,op,right,pos) ->
     check_opexp venv tenv frame left op right pos

and check_varexp venv tenv frame sym pos =
  match S.get venv sym with
  | None ->
     Error.undeclared_variable sym pos;
     let (reg,insts) = Tr.gen_nil frame in
     (reg,insts,T.Byte)
  | Some(E.FunEntry _) ->
     Error.variable_expected sym pos;
     let (reg,insts) = Tr.gen_nil frame in
     (reg,insts,T.Byte)
  | Some(E.VarEntry var) ->
     let (reg,insts) = Tr.gen_load_access frame var.E.typ var.E.access in
     (reg,insts,var.E.typ)

and check_callexp venv tenv frame sym params pos =
  match S.get venv sym with
  | None ->
     Error.undeclared_function sym pos;
     let (reg,insts) = Tr.gen_nil frame in
     (reg,insts,T.Unit)
  | Some(E.VarEntry _) ->
     Error.function_expected sym pos;
     let (reg,insts) = Tr.gen_nil frame in
     (reg,insts,T.Unit)
  | Some(E.FunEntry funentry) ->
     let params_insts = ref [] in
     let params_regs =
       List.map2
         (fun exp decl_param_typ ->
          let (reg,insts,typ) = check_exp venv tenv frame exp pos in
          assert_type typ decl_param_typ pos;
          params_insts := !params_insts@insts;
          (reg,typ))
         params funentry.E.params in
     let callparams_insts =
       List.mapi
         (fun index (reg,typ) -> Tr.gen_callparam frame index reg typ)
         params_regs in
     let insts = !params_insts@(List.flatten callparams_insts) in
     let (reg,call_insts) = Tr.gen_call
                               frame
                               funentry.E.return
                               funentry.E.label in
     (reg,insts@call_insts,funentry.E.return)

and check_opexp venv tenv frame left op right pos =
  let (src1,linsts,left) = check_exp venv tenv frame left pos
  and (src2,rinsts,right) = check_exp venv tenv frame right pos in
  assert_type left right pos;
  assert_number left pos;
  let typ = match op with
    | A.AddOp | A.SubOp | A.MulOp | A.DivOp -> left
    | A.EqOp -> T.Bool in
  let (reg,opinsts) = Tr.gen_op frame op left src1 src2 in
  (reg,rinsts@linsts@opinsts,typ)

and check_stmt venv tenv frame rettyp stmt =
  match stmt with
  | A.LetStmt (name,typ,exp,pos) ->
     check_letstmt venv tenv frame name typ exp pos
  | A.ReturnUnitStmt (pos) ->
     let insts = Tr.gen_retunit () in
     (insts,venv,tenv)
  | A.ReturnStmt (exp,pos) ->
     let (src,insts,return) = check_exp venv tenv frame exp pos in
     assert_type return rettyp pos;
     let insts = Tr.gen_ret src in
     (insts,venv,tenv)
  | A.SeqStmt (stmts,pos) ->
     let (insts,_,_) =
       List.fold_left
         (fun (prev_insts,venv,tenv) stmt ->
          let (insts,venv,tenv) = check_stmt venv tenv frame rettyp stmt in
          (prev_insts@insts,venv,tenv))
         ([],venv,tenv)
         stmts in
     (insts,venv,tenv)
  | A.IfStmt (cond,thenbody,pos) ->
     let (condreg,condinsts,condtyp) = check_exp venv tenv frame cond pos in
     assert_type condtyp T.Bool pos;
     let (theninsts,venv',tenv') = check_stmt venv tenv frame rettyp thenbody in
     let insts = Tr.gen_ifthen frame condreg theninsts in
     (* TODO phi *)
     (insts,venv,tenv)
  | A.IfElseStmt (cond,then_body,else_body,pos) ->
     let (condreg,condinsts,condtyp) = check_exp venv tenv frame cond pos in
     assert_type condtyp T.Bool pos;
     let (theninsts,_,_) =  check_stmt venv tenv frame rettyp then_body
     and (elseinsts,_,_) =  check_stmt venv tenv frame rettyp else_body in
     let insts = Tr.gen_ifthenelse frame condreg theninsts elseinsts in
     (* TODO phi *)
     (insts,venv,tenv)
  | A.IgnoreStmt (exp,pos) ->
     let (_,insts,exp_typ) = check_exp venv tenv frame exp pos in
     ignore exp_typ;
     (insts,venv,tenv)

and check_letstmt venv tenv frame name typ init pos =
  assert_unique venv name pos;
  let typ = actual_type tenv typ pos in
  match init with
    | None ->
       let reg = Tr.alloc_reg frame in
       let access = Tr.InReg reg in
       let entry = E.make_var typ access in
       let venv' = S.put venv name entry in
       ([],venv',tenv)
    | Some(init) ->
       let (reg,insts,exp_type) = check_exp venv tenv frame init pos in
       assert_type typ exp_type pos;
       let access = Tr.InReg reg in
       let entry = E.make_var typ access in
       let venv' = S.put venv name entry in
       (insts,venv',tenv)

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
  | None   -> ()
  | Some _ -> Error.assert_unique sym pos

and assert_number typ pos =
  match typ with
  | T.Int | T.Byte -> ()
  | T.Unit | T.Nil | T.Bool -> Error.assert_number pos
