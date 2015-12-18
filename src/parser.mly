%{
  open Types
%}

%token <float> FLOAT
%token <bool> BOOL
%token <char> AOP
%token <string> BOP
%token <string> COP
%token <string> ID
%token NOT
%token IF
%token THEN
%token ELSE
%token FST
%token SND
%token HEAD
%token TAIL
%token NULL
%token CONS
%token LET
%token EQUAL
%token EQUALITY
%token IN
%token EOL
%token ARROW
%token LAMBDA
%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK
%token COMMA
%token MINUS
%token SEQUENCE
%nonassoc LET
%nonassoc IN
%left AOP BOP COP
%left EQUALITY
%left IF THEN ELSE
%right FST SND NOT HEAD TAIL CONS
%nonassoc LPAREN LBRACK
%nonassoc COMMA
%nonassoc ID
%nonassoc FLOAT BOOL NULL
%nonassoc MINUS
%nonassoc APP
%nonassoc SEQUENCE
%start main
%type <Types.resultS> main
%%

main:
  | headEx EOL                      { $1 }
;

headEx:
  | expr %prec LET                  { $1 }
  | LET ID EQUAL headEx IN headEx   { LetS ($2,$4,$6) }
;

expr:
  | FLOAT                           { NumS $1 }
  | BOOL                            { BoolS $1 }
  | expr AOP expr                   { AopS ($2,$1,$3) } 
  | expr BOP expr                   { BopS ($2,$1,$3) }
  | expr COP expr                   { CopS ($2,$1,$3) }
  | expr MINUS expr                 { AopS ('-',$1,$3) }
  | LPAREN MINUS expr RPAREN        { AopS ('-',NumS 0.,$3) }
  | expr EQUALITY expr              { EqualS ($1,$3) }
  | HEAD expr                       { HeadS $2 }
  | TAIL expr                       { TailS $2 }
  | NULL expr                       { EqualS ($2,ListS []) }
  | NOT expr                        { NotS $2 }
  | expr CONS expr                  { ConsS ($1,$3) }
  | LPAREN headEx COMMA headEx RPAREN   { ParS ($2,$4) }
  | LPAREN fundef RPAREN            { $2 } 
  | LPAREN headEx RPAREN            { $2 }
  | IF headEx THEN headEx ELSE headEx     { IfS ($2,$4,$6) }
  | ID                              { VarS $1 }
  | LBRACK RBRACK                   { ListS [] }
  | LBRACK expList RBRACK           { ListS ($2) }
  | FST expr                        { FstS $2 }
  | SND expr                        { SndS $2 }
  | expr SEQUENCE expr              { AppS ($3,$1) }
  | expr expr %prec APP             { AppS ($1,$2) }
;

fundef:
  | LAMBDA idlist ARROW headEx      { FunS (None,$2,$4) }
;

idlist:
  | ID idlist                       { $1 :: $2 }
  | ID                              { [$1] }
;

expList:
  | headEx                          { [$1] }
  | headEx COMMA expList            { $1 :: $3 }
;
