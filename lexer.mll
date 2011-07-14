{
  open Parser
}

let var = ['a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
    [' ' '\t' '\r' '\n']	{ token lexbuf }
  | ['0'-'9']+			{ INT (int_of_string(Lexing.lexeme lexbuf)) }
  | "int"			{ TINT }
  | "bool"			{ TBOOL }
  | "true"			{ TRUE }
  | "false"			{ FALSE }
  | "list"			{ LIST }
  | "fst"			{ FST }
  | "snd"			{ SND }
  | "out"			{ OUTF }
  | "fun"			{ FUN }
  | "is"			{ IS }
  | "in"			{ IN }
  | "if"			{ IF }
  | "then"			{ THEN }
  | "else"			{ ELSE }
  | "let"			{ LET }  
  | "and"			{ AND }  
  | "or"			{ OR }  
  | ";;"			{ SEMICOLON2 }
  | "::"			{ CONS }
  | '='				{ EQUAL }
  | '<'				{ LESS }
  | '>'				{ MORE }
  | "->"			{ TARROW }
  | ':'				{ COLON }
  | '('				{ LPAREN }
  | ')'				{ RPAREN }
  | '['				{ LBR }
  | ']'				{ RBR }
  | '+'				{ PLUS }
  | '-'				{ MINUS }
  | '*'				{ TIMES }
  | ','				{ PCOLON }
  | "const"                     { CONST }
  | var				{ VAR (Lexing.lexeme lexbuf) }
  | eof				{ EOF }

{
}
