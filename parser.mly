%{
  open Ast
%}

%token <int> INT
%token <string> ID
%token EOF PLUS MINUS MUL DIV
%token LPAREN RPAREN COMMA SEMICOLON LBRACK RBRACK RETURN EQ

%left PLUS MINUS
%left MUL DIV
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
  | t=id n=id LPAREN a=fun_args RPAREN LBRACK b=body RBRACK
        { FunDec (n,t,a,b,$startpos) }

fun_args:
  | a=separated_list(COMMA, fun_arg)
        { a }

fun_arg:
  | t=id n=id { (n,t) }

body:
  | l=list(stmt) { l }

var_decl:
  | t=id n=id SEMICOLON
        { VarDec (n,t,None,$startpos) }
  | t=id n=id EQ e=exp SEMICOLON
        { VarDec (n,t,Some e,$startpos) }

stmt:
  | var_decl
        { DeclStmt ($1,$startpos) }
  | RETURN SEMICOLON
        { ReturnStmt (NilExp ($startpos),$startpos) }
  | RETURN e=exp SEMICOLON
        { ReturnStmt (e,$startpos) }

exp:
  | INT
       { IntExp ($1,$startpos) }
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
  | i=id LPAREN l=separated_list(COMMA, exp) RPAREN
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
