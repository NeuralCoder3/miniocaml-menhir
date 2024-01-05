%{
open Ast
%}

// these tokens are also needed for the lexer

// keywords
%token IF THEN ELSE LET IN FUN REC
%token INT BOOL
// symbols
%token LP "("
%token RP ")"
%token ADD SUB MUL LEQ EQ ARROW
%token COLON
// constants and identifiers
%token <int> ICON
%token <string> VAR
%token <bool> BCON
// other
%token EOF

/* Types of exposed parser functions */

%type <Ast.program> program
%type <Ast.exp> topexpression

%start program
%start topexpression



// important that fun is lower than var
%nonassoc low
%nonassoc FUN LET IF
%nonassoc VAR LP ICON BCON
%left LEQ
%left ADD SUB
%left MUL
%left application

%%

pty :
  | INT { Int }
  | BOOL { Bool }
  | "(" t=ty ")" { t }
  ;

ty :
  | t1=pty ARROW t2=ty { Fun(t1, t2) }
  | pty { $1 }
  ;


sexpression:
  | i=ICON { Icon i } 
  | b=BCON { Bcon b }
  | x=VAR { Var x }
  | IF cond=expression THEN e1=expression ELSE e2=expression { If(cond, e1, e2) } %prec low
  | LET x=VAR EQ e1=expression IN e2=expression { Let(x, e1, e2) } %prec low
  | FUN "(" x=VAR COLON t=ty ")" ARROW e=expression { Lam(x, t, e) } %prec low
  | LET REC f=VAR "(" x=VAR COLON t=ty ")" COLON t2=ty EQ e1=expression IN e2=expression { Letrec(f, x, t, t2, e1, e2) } %prec low
  | "(" e=expression ")" { e }

expression:
  | e=sexpression { e } 
  | e=expression e2=expression { App(e, e2) } %prec application
  // we could explicitely encode associativity but menhir prec does it already
  | e1=expression LEQ e2=expression { Binary(Leq, e1, e2) }
  | e1=expression ADD e2=expression { Binary(Add, e1, e2) }
  | e1=expression SUB e2=expression { Binary(Sub, e1, e2) } 
  | e1=expression MUL e2=expression { Binary(Mul, e1, e2) } 
  ;

topexpression:
  | e=expression EOF { e }
  ;

program:
  | expression EOF { Expression $1 }
  ;
