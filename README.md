## Edsger's Caml
### A compiler for the Edsger programming language, written in OCaml

National Technical University of Athens
Compilers 2016, Flow L

_Achilles Benetopoulos_ & _Emmanouil Theodosis_ 

Dependencies:
- LLVM ocaml bindings
- LLVM 3.5
- clang

To build the compiler, run :

```bash
$ make depend
$ make
```
in the project's root directory. This will create a binary named "edsgerc".

To remove all intermediate compilation files :

```bash
$ make clean
```

To get the project back to its initial state :

```bash
$ make distclean
```

Should you need to rebuild the standard library, run the script 'libs.sh' inside the 'stdlib' folder.

To build any of the tests in the (aptly named) 'tests' folder, run:

```bash
$ ./edsgerc [options] [input_file]
```

from the root directory. Should compilation succeed, there will be an executable with the same name as the source
file provided, inside the 'tests' directory.
If an input file was not specified (using the -f or -i flags), then there will be an 'a.out' executable in the project's
root directory.
Calling the compiler with no options or an input file will display usage information.
