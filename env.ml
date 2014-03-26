type varentry = {typ:Types.t}

type ventry =
  | VarEntry of varentry
  | FunEntry of Types.t list * Types.t

let make_env lst : ventry Symbol.table =
  List.fold_left
    (fun symtable (sym, ty) -> Symbol.put symtable sym ty)
    Symbol.empty
    lst

let base_tenv : Types.t Symbol.table =
  List.fold_left
    (fun symtable (sym, ty) -> Symbol.put symtable sym ty)
    Symbol.empty
    [Symbol.symbol "int", Types.Int;
     Symbol.symbol "void", Types.Unit;
     Symbol.symbol "bool", Types.Bool]

let base_venv =
  Symbol.empty
