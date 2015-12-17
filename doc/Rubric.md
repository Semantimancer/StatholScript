# Rubric

StatholScript was made as part of a directed study at Hanover College. As such, a rubric for grading the language was also required.

  * Type-Checking
    * A
      * Typechecker can handle polymorphic types and recursion.
    * B
      * Typechecker can handle lists, pairs, and (if in the language) sequences.
    * C
      * Typechecker can handle let/if statements and function definition/application.
    * D
      * Typechecker can handle basic arithmetic, comparative, and boolean operators.
    * F
      * Language is not type-checked, or only type-checks constants.
  * Interpreter
    * A
      * Interpreter works on all expressions in the language.
      * Interpreter's environment includes a prompt with useful information.
      * Errors are always caught by the interpreter, rather than crashing.
    * B
      * Interpreter works on all expressions in the language.
      * Interpreter's environment includes a prompt.
      * Errors are always caught by the interpreter, rather than crashing.
    * C
      * Interpreter works on all expressions in the language.
      * Interpreter's environment is just blank space.
      * Errors in the code sometimes cause crashes, and sometimes are caught.
    * D
      * Interpreter works on most expressions in the language.
      * Interpreter's environment is just blank space.
      * Errors in the code cause crashes.
    * F
      * Interpreter does not work, or doesn't provide accurate answers to code.
  * Documentation
    * A
      * Description of the language and its semantics.
      * Good examples which illustrate the language's capabilities.
      * Formal CFG of the language's grammar (BNF).
    * B
      * Description of the language and its semantics.
      * Good examples which illustrate the language's capabilities.
    * C
      * Basic description of language.
      * Basic examples (nothing complex or note-worthy).
    * D
      * Basic description of language. Contains errors.
      * No examples.
    * F
      * No documentation.
  * Testing
    * SIDENOTE: Something is "handled properly" in the context of testing when it both
      works when it should and provides an appropriate error when it doesn't.
    * A
      * Tests show that polymorphism is handled properly.
      * Tests show that higher order functions can be written in the language.
    * B
      * Tests show that let/if statements and recursion are handled properly.
    * C
      * Tests show that function definition and application are handled properly.
    * D
      * Tests only consist of uses of arithmetic, boolean, and comparative operators.
    * F
      * No tests provided.
  * Language Features
    * A
      * Higher order functions (map, fold, filter) can be implemented in the language.
    * B
      * Language can operate on lists/pairs with no errors other than those that
        should be present (e.g. head []), which are caught properly.
    * C
      * Language can operate on lists/pairs, but with uncaught errors.
    * D
      * Language has pair and/or list constructions, but no functions to use on them.
    * F
      * Language has no language features.
