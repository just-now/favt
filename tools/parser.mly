%{
  open Syntax
  exception ByUser
  let pf_elist = function Args x -> x  | _ -> []
  let mv_args = function Args x -> List.hd x | _ -> raise ByUser
  let cp_args = function Args x -> List.hd x | z -> z
%}

%token TINT
%token TBOOL
%token TARROW
%token <Syntax.name> VAR
%token <int> INT
%token TRUE FALSE
%token OR AND MORE
%token PLUS
%token MINUS
%token TIMES
%token EQUAL LESS
%token IF THEN ELSE
%token FUN IS IN
%token COLON
%token LPAREN RPAREN
%token LET
%token SEMICOLON2
%token PCOLON
%token EOF
%token CONS
%token FST
%token SND
%token OUTF
%token RBR LBR
%token LIST
%token CONST
%start toplevel
%type <Syntax.toplevel_cmd list> toplevel

%nonassoc FUN IS
%nonassoc IF THEN ELSE
%nonassoc EQUAL LESS MORE
%left PLUS MINUS OR AND
%left TIMES
%left COLON
%left PCOLON
%right CONS
%right TARROW

%%

toplevel:
    EOF                      { [] }
  | def EOF                  { [$1] }
  | def SEMICOLON2 EOF       { [$1] }
  | expr EOF                 { [Expr $1] }
  | expr SEMICOLON2 EOF      { [Expr $1] }
  | def SEMICOLON2 toplevel  { $1 :: $3 }
  | expr SEMICOLON2 toplevel { (Expr $1) :: $3 }

def: 
    LET VAR EQUAL expr { Def ($2, $4) }
  | LET VAR EQUAL LPAREN expr RPAREN { Def ($2, $5) }
  | LET CONST VAR EQUAL expr { CDef ($3, $5) }
  | LET CONST VAR EQUAL LPAREN expr RPAREN { CDef ($3, $6) }


expr:
    non_app             { $1 }
  | app                 { $1 }
  | arith               { $1 }
  | boolean             { $1 }
  | expr CONS expr      { Cons (cp_args $1, cp_args $3) }
  | expr AND expr	{ And (cp_args $1, cp_args $3) }
  | expr OR expr	{ Or (cp_args $1, cp_args $3) }
  | IF expr THEN expr ELSE expr	{ If ($2, $4, $6) }
  | FUN VAR LPAREN funcargs RPAREN COLON ty IS expr { Fun ($2, $4, $7, $9) }
  | LET VAR EQUAL expr IN expr { Let ($2, $4, $6) }

funcargs:
    VAR COLON ty			{ [($1,$3)] }
  | funcargs PCOLON VAR COLON ty        { $1 @ [($3,$5)]}

app:
    FST non_app        { Fst (mv_args $2) }
  | SND non_app        { Snd (mv_args $2) }
  | OUTF non_app       { Out (mv_args $2) }
  | app non_app        { Apply ($1, $2) }
  | non_app non_app    { Apply ($1, $2) }

non_app:
    VAR					{ Var $1 }
  | TRUE				{ Bool true }
  | FALSE				{ Bool false }
  | INT					{ Int $1 }
  | nil					{ $1 }
  | LPAREN expr RPAREN			{ Args [$2] }
  | LPAREN exprs RPAREN			{ $2 }

nil:
    LBR RBR		{ Nil }

exprs:
    expr PCOLON expr   { Args [$1;$3] }
  | expr PCOLON exprs  { Args ([$1] @ (pf_elist $3)) }

arith:
  | MINUS INT           { Int (-$2) }
  | expr PLUS expr	{ Plus ($1, $3) }
  | expr MINUS expr	{ Minus ($1, $3) }
  | expr TIMES expr	{ Times ($1, $3) }

boolean:
  | expr EQUAL expr { Equal ($1, $3) }
  | expr LESS expr  { Less ($1, $3) }
  | expr MORE expr  { More ($1, $3) }

ty:
    TBOOL	 { TBool }
  | TINT         { TInt }
  | ty TARROW ty { TArrow ($1, $3) }
  | LIST ty      { TCons $2 }
  | LPAREN ty RPAREN { $2 }

%%

