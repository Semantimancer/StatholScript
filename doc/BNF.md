     <statement> ::= <head-exp>

      <head-exp> ::= <expr> | "let" <id> "=" <head-exp> "in" <head-exp>

          <expr> ::= <float> | <bool> | <char> | <expr> <aop> <expr> | <expr> <bop> <expr>
                   | <expr> <cop> <expr> | <expr> "-" <expr> | "(" "-" <expr> ")"
                   | <expr> "==" <expr> | "head" <expr> | "tail" <expr> | "null?" <expr>
                   | <not> <expr> | <expr> ":" <expr> | "(" <head-exp> "," <head-exp> ")"
                   | "(" <fundef> ")" | "(" <head-exp> ")" | "fst" <expr> | "snd" <expr>
                   | "if" <head-exp> "then" <head-exp> "then" <head-exp> | "[]"
                   | "[" <exp-list> "]" | <expr> <expr>

           <aop> ::= "+" | "*" | "/"

           <bop> ::= "&&" | "||"

           <cop> ::= ">" | "<" | "=>" | "<=" | ">=" | "=<"

           <not> ::= "not" | "!"

        <fundef> ::= "\" <id-list> "->" <head-exp>

       <id-list> ::= <id> | <id> <id-list>

      <exp-list> ::= <head-exp> | <head-exp> "," <exp-list>

          <bool> ::= "true" | "false"

         <float> ::= <num> | <num> <exp>

          <char> ::= "'" <character> "'"

           <exp> ::= "e" <digits> | "E" <digits> | "e-" <digits> | "E-" <digits>

           <num> ::= <digits> | <digits> "." | <digits> "." <digits>

        <digits> ::= <digit> | <digit> <digits>

         <digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
