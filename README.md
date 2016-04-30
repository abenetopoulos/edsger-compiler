Compilers 2016, Flow L;
National Technical University of Athens;
Achilles Benetopoulos 031 12 614;
Emmanouil Theodosis   031 12 026;

The compiler is written in OCaml. Any further general comments will be added here.

To build the compiler (lexer and parser at the moment), run:
$ make all
in the project's root directory. This will create a binary named "edsgerc".

To remove all intermediate compilation files :
$ make clean

To get the project back to its initial state :
$ make distclean

To automatically run all the tests, simply type:
$ ./run_tests.sh
in the project's root directory. If a test fails (as is the case with "bad_test.eds"), an appropriate
 message is displayed.

** NOTES ON THE PARSER **
The parser's error messages are a little less than helpful at the moment. This will be fixed in the future.

While building the project, you might notice the rather large amount of conflicts encountered by the parser.

Shift/reduce in state 106: Harmless conflict, and shifting is the desired behaviour. While parsing a parameter
list, we don't want to treat the ',' character as a binary operator, but as a parameter list separator.

Shift/reduce in state 45: Once again, shifting is the desired behaviour, since we would like to consider the
array expression following a "new" expression to be part of said "new" expression.

All the other conflicts originate from our need to be able to parse expressions of the following kind:

    a = new {type} {0 or more '*'} '*' expression;

which, although semantically invalid, are syntactically valid. Due to this fact, we had to change the way the
"new" expressions are produced, generating a number of conflicts in the process, none of which (seemingly) cause 
us any issues.
