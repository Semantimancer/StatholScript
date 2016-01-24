{
  open Parser
  exception Eof
  exception Unrecognized
}

let any = _
let digit = ['0'-'9']
let sign = ['+' '-']
let frac = '.' digit+
let exp = ['e' 'E'] sign? digit+
let white = [' ' '\t']+ | "\\\n" | "//" ([^ '\n' '\r'])*
let newline = '\n' | '\r' | "\r\n"
let semicolon = ';'
let letter = ['a'-'z' 'A'-'Z']
let alphanum = letter | digit
let lparen = '('
let rparen = ')'
let lbrack = '['
let rbrack = ']'
let comma = ','
let aOp = '+' | '*' | '/'
let bOp = "&&" | "||"
let cOp = ">" | "<" | "=>" | "<=" | ">=" | "=<"

let float = (digit+ '.'? | digit* frac) exp?
let bool = "true" | "false"
let char = ''' any '''

let arrow = "->"
let sequence = "|>"
let minus = '-'
let equal = '='
let equal' = "=="
let notS = "not" | "!"
let letS = "let"
let inS = "in"
let lambda = '\\'
let ifS = "if"
let thenS = "then"
let elseS = "else"
let fstS = "fst"
let sndS = "snd"
let head = "head"
let tail = "tail"
let cons = ":"
let null = "null?"
let id = letter alphanum*

rule token = parse
  | white       { token lexbuf }
  | newline     { token lexbuf }
  | semicolon   { EOL }
  | float as x  { FLOAT (float_of_string x) }
  | bool as x   { BOOL (bool_of_string x) }
  | char as x   { CHAR (String.get x 1) }
  | minus       { MINUS }
  | aOp as x    { AOP x }
  | bOp as x    { BOP x }
  | cOp as x    { COP x }
  | equal'      { EQUALITY }
  | fstS        { FST }
  | sndS        { SND }
  | head        { HEAD }
  | tail        { TAIL }
  | cons        { CONS }
  | null        { NULL }
  | notS        { NOT }
  | letS        { LET }
  | inS         { IN }
  | ifS         { IF }
  | thenS       { THEN }
  | elseS       { ELSE }
  | arrow       { ARROW }
  | sequence    { SEQUENCE }
  | lambda      { LAMBDA }
  | lparen      { LPAREN }
  | rparen      { RPAREN }
  | lbrack      { LBRACK }
  | rbrack      { RBRACK }
  | comma       { COMMA }
  | equal       { EQUAL }
  | id as s     { ID s }
  | eof         { raise Eof }
  | any         { raise Unrecognized }
