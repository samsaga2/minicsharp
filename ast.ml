type symbol = Symbol.t
type typeid = Symbol.t
type pos = Lexing.position

type prog = decl list
 and decl =
   | FunDec of symbol * symbol * (symbol * symbol) list * stmt * pos
   | VarDec of symbol * symbol * exp option * pos
 and stmt =
   | LetStmt of symbol * symbol * exp option * pos
   | ReturnUnitStmt of pos
   | ReturnStmt of exp * pos
   | SeqStmt of stmt list * pos
   | IfStmt of exp * stmt * pos
   | IfElseStmt of exp * stmt * stmt * pos
   | IgnoreStmt of exp * pos
 and exp =
   | NilExp of pos
   | IntExp of int * pos
   | ByteExp of int * pos
   | BoolExp of bool * pos
   | VarExp of symbol * pos
   | CallExp of symbol * exp list * pos
   | OpExp of exp * op * exp * pos
 and op =
   | AddOp | SubOp | MulOp | DivOp
   | EqOp
