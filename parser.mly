%{
  open LMJ
  let swap = List.map (fun (x, y) -> (y, x))
%}

%token <int32> INT_CONST
%token <bool> BOOL_CONST
%token INTEGER BOOLEAN
%token LET MUT ADDR
%token <string Location.t> IDENT
%token FUNC MAIN RETURN
%token PLUS MINUS TIMES NOT LT AND
%token COMMA SEMICOLON COLON
%token ASSIGN RET_TYPE
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token SELF NEW DOT
%token SYSO
%token IF ELSE WHILE
%token EOF

%left AND
%nonassoc LT
%left PLUS MINUS
%left TIMES
%nonassoc NOT
%nonassoc DOT LBRACKET

%start program

%type <LMJ.program> program

%%

program:
| m = main_function d = defs EOF
   {
      let i=m in
      {
         defs = d;
         main = m;
      }
   }

main_function:
| FUNC MAIN LPAREN RPAREN
   LBRACE
   i = instruction
   RBRACE
   { i }

defs:
| c = list(functio)
   { c }

functio:
| FUNC name = IDENT
   LPAREN
   f = separated_list(COMMA, separated_pair(IDENT, COLON, typ))
   RPAREN
   RET_TYPE t = typ
   LBRACE
   ds = declarations_and_statements
   RETURN e = expression SEMICOLON
   RBRACE
   {
     let d, s = fst ds, snd ds in
     name,
     {
       formals = f;
       result  = t;
       locals  = d;
       body    = s;
       return  = e;
     }
   }

declarations_and_statements:
| LET id = IDENT COLON t = typ SEMICOLON r = declarations_and_statements
   {
     let d, s = r in
     ((id, t) :: d, s)
   }
| s = list(instruction)
   { ([], s) }

expression:
|  e = raw_expression
   { Location.make $startpos $endpos e }
| LPAREN e = expression RPAREN
   { e }

raw_expression:
| i = INT_CONST
   { EConst (ConstInt i) }

| b = BOOL_CONST
   { EConst (ConstBool b) }

| id = IDENT
   { EGetVar id }

| e1 = expression op = binop e2 = expression
   { EBinOp (op, e1, e2) }

// | o = expression DOT c = IDENT LPAREN actuals = separated_list(COMMA, expression) RPAREN
//    { EMethodCall (o, c, actuals) }

// | a = expression LBRACKET i = expression RBRACKET
//    { EArrayGet (a, i) }

// | NEW INTEGER LBRACKET e = expression RBRACKET
//    { EArrayAlloc e }

// | a = expression DOT LENGTH
//    { EArrayLength a }

// | SELF
//    { ESelf }

// | NEW id = IDENT LPAREN RPAREN
//    { EObjectAlloc id }

| NOT e = expression
   { EUnOp (UOpNot, e) }

%inline binop:
| PLUS  { OpAdd }
| MINUS { OpSub }
| TIMES { OpMul }
| LT    { OpLt }
| AND   { OpAnd }

instruction:
| b = block
   { b }

| id = IDENT ASSIGN e = expression SEMICOLON
   { ISetVar (id, e) }

// | a = IDENT LBRACKET i = expression RBRACKET ASSIGN e = expression SEMICOLON
//    { IArraySet (a, i, e) }

| SYSO LPAREN e = expression RPAREN SEMICOLON
   { ISyso e }

| IF LPAREN c = expression RPAREN i1 = instruction ELSE i2 = instruction
   { IIf (c, i1, i2) }

| WHILE LPAREN c = expression RPAREN i = instruction
   { IWhile (c, i) }

block:
| LBRACE is = list(instruction) RBRACE
   { IBlock is }

typ:
| INTEGER
   { TypInt }
| BOOLEAN
   { TypBool }
// | LBRACKET typ COMMA INTEGER RBRACKET
//    { TypIntArray }
| id = IDENT
   { Typ id }
