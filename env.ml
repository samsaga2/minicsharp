type varentry = {typ:Types.t; access:Translate.access}

type funentry = {label: Symbol.symbol; params:Types.t list; return:Types.t}

type ventry =
  | VarEntry of varentry
  | FunEntry of funentry

let make_fun label params return =
  FunEntry {label=label; params=params; return=return}

let make_var typ access =
  VarEntry {typ=typ; access=access}

let extend_env env lst =
  List.fold_left Symbol.put_pair env lst

let make_env lst =
  extend_env Symbol.empty lst

let base_tenv =
  make_env
    [Symbol.symbol "int", Types.Int;
     Symbol.symbol "void", Types.Unit;
     Symbol.symbol "bool", Types.Bool]

let base_venv =
  Symbol.empty
