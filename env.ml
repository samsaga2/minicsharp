module S = Symbol
module T = Types

type varentry = {typ:T.t; access:Translate.access}

type funentry = {label: S.t; params:T.t list; return:T.t}

type ventry =
  | VarEntry of varentry
  | FunEntry of funentry

let make_fun label params return =
  FunEntry {label=label; params=params; return=return}

let make_var typ access =
  VarEntry {typ=typ; access=access}

let extend_env env lst =
  List.fold_left S.put_pair env lst

let make_env lst =
  extend_env S.empty lst

let base_tenv =
  make_env
    [S.symbol "byte" , T.Byte;
     S.symbol "int"  , T.Int;
     S.symbol "void" , T.Unit;
     S.symbol "bool" , T.Bool]

let base_venv =
  S.empty
