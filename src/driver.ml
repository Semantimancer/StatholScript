open Types

let _ =
  let lexbuf = Lexing.from_channel stdin in
    while true do
      output_string stdout ">> ";
      flush stdout;
      try 
        let result = Parser.main Lexer.token lexbuf in
          output_string stdout @@ valToString @@ evaluate result; print_newline();
          output_string stdout "{";
          output_string stdout @@ tyToString @@ typecheck (desugar result);
          output_string stdout "}"
      with
        | Parsing.Parse_error       -> output_string stdout "Parse error in statement";
                                       print_newline();
                                       Lexing.flush_input lexbuf
        | Lexer.Eof                 -> output_string stdout "Exit code received"; 
                                       exit 0
        | Lexer.Unrecognized        -> output_string stdout "Unrecognized token error";
                                       print_newline();
                                       Lexing.flush_input lexbuf
        | Types.Typecheck_failed s  -> output_string stdout "\nTypecheck error: ";
                                       output_string stdout s;
                                       print_newline();
        | Types.Desugar_failed      -> output_string stdout "Error in desugarer";
                                       print_newline();
                                       Lexing.flush_input lexbuf
        | Types.Self_referential    -> output_string stdout "Self-referential error";
                                       print_newline();
                                       Lexing.flush_input lexbuf
        | _                         -> output_string stdout 
                                        "You've encountered an unexpected error";
                                       print_newline();
                                       Lexing.flush_input lexbuf
    done
