type varentry = {typ:Types.t}

type funentry = {label: Symbol.symbol; args:Types.t list; rettype: Types.t}

type ventry =
  | VarEntry of varentry
  | FunEntry of funentry

let make_env lst =
  List.fold_left Symbol.put_pair Symbol.empty lst

let base_tenv =
  make_env
    [Symbol.symbol "int", Types.Int;
     Symbol.symbol "void", Types.Unit;
     Symbol.symbol "bool", Types.Bool]

let base_venv =
  Symbol.empty
