type ventry =
  | VarEntry of Types.t
  | FunEntry of Types.t list * Types.t

let make_env lst =
  List.fold_left
    (fun symtable (sym, ty) -> Symbol.put symtable sym ty)
    Symbol.empty
    lst

let base_tenv =
  List.fold_left
    (fun symtable (sym, ty) -> Symbol.put symtable sym ty)
    Symbol.empty
    [Symbol.symbol "int", Types.Int]

let base_venv =
  Symbol.empty
