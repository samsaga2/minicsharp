%{
  open Ast
%}

%token <int> INT
%token <int> BYTE
%token <string> ID
%token EOF PLUS MINUS MUL DIV
%token LPAREN RPAREN COMMA SEMICOLON LBRACK RBRACK RETURN
%token EQ DEQ
%token IF ELSE
%token TRUE FALSE

%nonassoc IFX
%nonassoc ELSE
%left PLUS MINUS
%left MUL DIV
%nonassoc DEQ
%nonassoc UMINUS

%start <Ast.prog> program

%%

program:
  | l=list(decl) EOF
        { l }

decl:
  | fun_decl { $1 }
  | var_decl { $1 }

fun_decl:
  | t=id n=id LPAREN a=fun_args RPAREN b=body
        { FunDec (n,t,a,b,$startpos) }

fun_args:
  | a=separated_list(COMMA, fun_arg)
        { a }

fun_arg:
  | t=id n=id { (n,t) }

body:
  | LBRACK l=list(stmt) RBRACK
        { SeqStmt (l,$startpos) }

var_decl:
  | t=id n=id SEMICOLON
        { VarDec (n,t,None,$startpos) }
  | t=id n=id EQ e=exp SEMICOLON
        { VarDec (n,t,Some e,$startpos) }

stmt:
  | t=id n=id SEMICOLON
        { LetStmt (n,t,None,$startpos) }
  | t=id n=id EQ e=exp SEMICOLON
        { LetStmt (n,t,Some e,$startpos) }
  | RETURN SEMICOLON
        { ReturnUnitStmt ($startpos) }
  | RETURN e=exp SEMICOLON
        { ReturnStmt (e,$startpos) }
  | body
        { $1 }
  | e=exp SEMICOLON
        { IgnoreStmt (e,$startpos) }
  | IF LPAREN e=exp RPAREN b=stmt %prec IFX
        { IfStmt (e,b,$startpos) }
  | IF LPAREN e=exp RPAREN b=stmt ELSE l=stmt
        { IfElseStmt (e,b,l,$startpos) }

exp:
  | INT
       { IntExp ($1,$startpos) }
  | BYTE
       { ByteExp ($1,$startpos) }
  | TRUE
       { BoolExp (true,$startpos) }
  | FALSE
       { BoolExp (false,$startpos) }
  | id
       { VarExp ($1,$startpos) }
  | MINUS e=exp %prec UMINUS
       { OpExp (IntExp (0,$startpos),SubOp,e,$startpos) }
  | exp_op
       { $1 }
  | LPAREN RPAREN
       { NilExp ($startpos) }
  | LPAREN e=exp RPAREN
       { e }
  | call
       { $1 }

call:
  | i=id LPAREN l=separated_list(COMMA,exp) RPAREN
       { CallExp (i,l,$startpos) }

id:
  | ID
       { Symbol.symbol $1 }

exp_op:
  | left=exp PLUS right=exp
       { OpExp (left,AddOp,right,$startpos) }
  | left=exp MINUS right=exp
       { OpExp (left,SubOp,right,$startpos) }
  | left=exp MUL right=exp
       { OpExp (left,MulOp,right,$startpos) }
  | left=exp DIV right=exp
       { OpExp (left,DivOp,right,$startpos) }
  | left=exp DEQ right=exp
       { OpExp (left,EqOp,right,$startpos) }
