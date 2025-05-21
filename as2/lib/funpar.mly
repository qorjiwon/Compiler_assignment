%{
module A = Absyn
module S = Symbol

let start_pos = Parsing.symbol_start
let end_pos = Parsing.symbol_end
%}

%start prog

%token COMMA SEMICOLON COLON
%token LPAREN RPAREN
%token PLUS MINUS TIMES
%token LT EQ GT
%token AND NOT OR
%token WHILE DO REF BANG ASSIGN
%token IF THEN ELSE
%token LET IN FUN ARROW
%token EOF
%token <string> ID
%token <int> NUM PROJ

%type <Absyn.prog> prog
%type <Absyn.exp> exp add_exp assign_exp call_exp compare_exp mul_exp not_exp primary_exp seq_exp unary_exp logic_and_exp logic_or_exp
%type <Absyn.exp list> exp_list
%type <Absyn.tp> tp
%type <Absyn.tp list> tp_list
%type <Absyn.fundec> decl
%type <Absyn.fundec list> decl_list

%nonassoc IFX
%nonassoc ELSE
%left SEMICOLON
%right ASSIGN
%right COLON
%left OR
%left AND
%nonassoc NOT
%left EQ LT GT
%left PLUS MINUS
%left TIMES
%right UMINUS

%%

prog:
  decl_list EOF { $1 }
;

decl_list:
  decl { [$1] }
| decl decl_list { $1 :: $2 }
;

decl:
  FUN ID LPAREN ID COLON tp RPAREN COLON tp EQ exp {
    let pos = (start_pos (), end_pos ()) in
    pos, (S.symbol $2, S.symbol $4, $6, $9, $11)
  }
;

tp:
  ID                     { A.Inttp }
| tp ARROW tp            { A.Arrowtp($1, $3) }
| tp REF                 { A.Reftp($1) }
| LT GT                  { A.Tupletp([]) }
| LT tp_list GT          { A.Tupletp($2) }
| LPAREN tp RPAREN       { $2 }
;

tp_list:
  tp                     { [$1] }
| tp COMMA tp_list       { $1 :: $3 }
;

primary_exp:
  NUM                    { A.Int($1) }
| ID                     { A.Id(S.symbol $1) }
| PROJ primary_exp       { A.Proj($1, $2) }
| LPAREN exp RPAREN      { $2 }
| LT GT                  { A.Tuple([]) }
| LT exp_list GT         { A.Tuple($2) }
;

call_exp:
  primary_exp                             { $1 }
| call_exp LPAREN exp RPAREN              { A.Call($1, $3) }
| call_exp LPAREN LT GT RPAREN            { A.Call($1, A.Tuple([])) }
;

unary_exp:
  MINUS unary_exp %prec UMINUS             { A.Op(A.Sub, [A.Int(0); $2]) }
| BANG unary_exp                          { A.Op(A.Get, [$2]) }
| REF unary_exp                           { A.Op(A.Ref, [$2]) }
| call_exp                                { $1 }
;

mul_exp:
  mul_exp TIMES unary_exp                 { A.Op(A.Mul, [$1; $3]) }
| unary_exp                               { $1 }
;

add_exp:
  add_exp PLUS mul_exp                    { A.Op(A.Add, [$1; $3]) }
| add_exp MINUS mul_exp                   { A.Op(A.Sub, [$1; $3]) }
| mul_exp                                 { $1 }
;

compare_exp:
  add_exp LT add_exp                      { A.Op(A.LT, [$1; $3]) }
| add_exp EQ add_exp                      { A.Op(A.Eq, [$1; $3]) }
| add_exp                                 { $1 }
;

not_exp:
  NOT not_exp                             { A.If($2, A.Int(0), A.Int(1)) }
| compare_exp                             { $1 }
;

logic_and_exp:
  logic_and_exp AND not_exp               { A.If($1, $3, A.Int(0)) }
| not_exp                                 { $1 }
;

logic_or_exp:
  logic_or_exp OR logic_and_exp           { A.If($1, A.Int(1), $3) }
| logic_and_exp                           { $1 }
;

assign_exp:
  logic_or_exp ASSIGN assign_exp          { A.Op(A.Set, [$1; $3]) }
| logic_or_exp                            { $1 }
;

seq_exp:
  seq_exp SEMICOLON seq_exp               { A.Let(S.symbol "_", $1, $3) }
| assign_exp                              { $1 }
;

exp:
  seq_exp                                 { A.Pos((start_pos (), end_pos ()), $1) }
| IF exp THEN exp %prec IFX               { A.Pos((start_pos (), end_pos ()), A.If($2, $4, A.Int(0))) }
| IF exp THEN exp ELSE exp                { A.Pos((start_pos (), end_pos ()), A.If($2, $4, $6)) }
| WHILE exp DO exp                        { A.Pos((start_pos (), end_pos ()), A.Let(S.symbol "_", A.While($2, $4), A.Tuple([]))) }
| LET ID EQ exp IN exp                    { A.Pos((start_pos (), end_pos ()), A.Let(S.symbol $2, $4, $6)) }
| LET ID COLON tp EQ exp IN exp           { A.Pos((start_pos (), end_pos ()), A.Let(S.symbol $2, A.Constrain($6, $4), $8)) }
| exp COLON tp %prec COLON                { A.Pos((start_pos (), end_pos ()), A.Constrain($1, $3)) }
;

exp_list:
  exp                                     { [$1] }
| exp COMMA exp_list                      { $1 :: $3 }
;

%%