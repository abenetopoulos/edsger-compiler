Compilers 2016, Flow L;
National Technical University of Athens;
Achilles Benetopoulos 031 12 614;
Emmanouil Theodosis   031 12 026;

Our compiler will be in OCaml. Any further general comments will be added here.

** Lexer implementation **
In order to compile the lexer, you need to issue the following command from the project's root directory:
$ make
which will produce an executable called 'lexer', which you can invoke by typing:
$ ./lexer [file]
If no input file is provided, the program expects user input from stdin.

We've also created a test set. The file "should_work.eds" should pass the lexical analysis correctly. Every other file should raise an error, or behave according to the comments inside it. We've also included two header files:
- stdio.h: the standard included file in "should_work.eds" and should work without raising errors.
- loop.h: you can swap this with the "stdio.h" in "should_work.eds" and you should get an error about circular includes.

For reference, here's the complete list of our test set:
- char.eds
- comment.eds
- invalid_inc.eds
- loop.h
- multiline.eds
- name.eds
- not_first.eds
- real_comma.eds
- real_exp.eds
- real_exp2.eds
- self_loop.eds
- should_work.eds
- stdio.h

To automatically run all the tests, simply type:
$ ./run_tests.sh
in the project's root directory. This will create a directory named "outputs" inside the "tests" folder, and populate it with the results of running the lexical analyzer on each of the input files.

To remove all intermediate lexer compilation files :
$ make clean

To get the project back to its initial state :
$ make distclean
