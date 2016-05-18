Compilers 2016, Flow L;
National Technical University of Athens;
Achilles Benetopoulos 031 12 614;
Emmanouil Theodosis   031 12 026;

The compiler is written in OCaml. Any further general comments will be added here.

To build the compiler, run :
$ make depend
$ make
in the project's root directory. This will create a binary named "edsgerc".

To remove all intermediate compilation files :
$ make clean

To get the project back to its initial state :
$ make distclean

To automatically run all the tests, simply type:
$ ./run_tests.sh
in the project's root directory. If a test fails (as is the case with "bad_bsort.eds" and "bad_hanoi.eds"), an appropriate
message is displayed.

NOTES:
At the moment, the warning/error messages that are produced during semantic analysis aren't very helpful.
We report the problem's nature (by means of an exception in most cases), without specifying any information
which would aid the programmer in fixing the problems being reported (file, line number etc.). This will be fixed in the future.
