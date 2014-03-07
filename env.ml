type ventry =
  | VarEntry of Types.t
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
     Symbol.symbol "void", Types.Unit]

let base_venv =
  Symbol.empty
