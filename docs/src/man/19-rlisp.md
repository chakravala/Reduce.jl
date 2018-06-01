# 19 REDUCE and Rlisp Utilities

REDUCE and its associated support language system Rlisp include a number of utilities which have proved useful for program development over the years. The following are supported in most of the implementations of REDUCE currently available.

```@contents
Pages = ["19-rlisp.md"]
```

## 19.1 The Standard Lisp Compiler

Many versions of REDUCE include a Standard Lisp compiler that is automatically loaded on demand. You should check your system specific user guide to make sure you have such a compiler. To make the compiler active, the switch `comp` should be turned on. Any further definitions input after this will be compiled automatically. If the compiler used is a derivative version of the original Griss-Hearn compiler, (M. L. Griss and A. C. Hearn, “A Portable LISP Compiler", SOFTWARE — Practice and Experience 11 (1981) 541-605), there are other switches that might also be used in this regard. However, these additional switches are not supported in all compilers. They are as follows:

`plap`
- If `on`, causes the printing of the portable macros produced by the compiler;

`pgwd`
- If `on`, causes the printing of the actual assembly language instructions generated from the macros;

`pwrds`
- If `on`, causes a statistic message of the form 
```
⟨function⟩ COMPILED, ⟨words⟩ WORDS, ⟨words⟩ LEFT 
```
to be printed. The first number is the number of words of binary program space the compiled function took, and the second number the number of words left unused in binary program space.

## 19.2 Fast Loading Code Generation Program

In most versions of REDUCE, it is possible to take any set of Lisp, Rlisp or REDUCE commands and build a fast loading version of them. In Rlisp or REDUCE, one does the following:
```
         faslout <filename>;  
         <commands or IN statements>  
         faslend;
```
To load such a file, one uses the command `load`, e.g. `load foo;` or `load foo,bah;`

This process produces a fast-loading version of the original file. In some implementations, this means another file is created with the same name but a different extension. For example, in PSL-based systems, the extension is `b` (for binary). In CSL-based systems, however, this process adds the fast-loading code to a single file in which all such code is stored. Particular functions are provided by CSL for managing this file, and described in the CSL user documentation.

In doing this build, as with the production of a Standard Lisp form of such statements, it is important to remember that some of the commands must be instantiated during the building process. For example, macros must be expanded, and some property list operations must happen. The REDUCE sources should be consulted for further details on this.

To avoid excessive printout, input statements should be followed by a `$` instead of the semicolon. With `load` however, the input doesn’t print out regardless of which terminator is used with the command.

If you subsequently change the source files used in producing a fast loading file, don’t forget to repeat the above process in order to update the fast loading file correspondingly. Remember also that the text which is read in during the creation of the fast load file, in the compiling process described above, is *not* stored in your REDUCE environment, but only translated and output. If you want to use the file just created, you must then use `load` to load the output of the fast-loading file generation program.

When the file to be loaded contains a complete package for a given application, `load_package` rather than `load` should be used. The syntax is the same. However, `load_package` does some additional bookkeeping such as recording that this package has now been loaded, that is required for the correct operation of the system.

## 19.3 The Standard Lisp Cross Reference Program

`cref` is a Standard Lisp program for processing a set of Standard LISP function definitions to produce:

1. A “summary” showing:
  * A list of files processed;
  * A list of “entry points” (functions which are not called or are only called by themselves);
  * A list of undefined functions (functions called but not defined in this set of functions);
  * A list of variables that were used non-locally but not declared `global` or `fluid` before their use;
  * A list of variables that were declared `global` but not used as `fluid`s, i.e., bound in a function;
  * A list of `fluid` variables that were not bound in a function so that one might consider declaring them `global`s;
  * A list of all `global` variables present;
  * A list of all `fluid` variables present;
  * A list of all functions present.
2. A “global variable usage” table, showing for each non-local variable:
  * Functions in which it is used as a declared `fluid` or `global`;
  * Functions in which it is used but not declared;
  * Functions in which it is bound;
  * Functions in which it is changed by `setq`.
3. A “function usage” table showing for each function:
  * Where it is defined;
  * Functions which call this function;
  * Functions called by it;
  * Non-local variables used.

The program will also check that functions are called with the correct number of arguments, and print a diagnostic message otherwise.

The output is alphabetized on the first seven characters of each function name.

### 19.3.1 Restrictions

Algebraic procedures in REDUCE are treated as if they were symbolic, so that algebraic constructs will actually appear as calls to symbolic functions, such as AEVAL.
### 19.3.2 Usage

To invoke the cross reference program, the switch `cref` is used. `on cref` causes the `cref` program to load and the cross-referencing process to begin. After all the required definitions are loaded, `off cref` will cause the cross-reference listing to be produced. For example, if you wish to cross-reference all functions in the file `tst.red`, and produce the cross-reference listing in the file `tst.crf`, the following sequence can be used:
```
        out ~tst.crf~;  
        on cref;  
        in ~tst.red~$  
        off cref;  
        shut ~tst.crf~;
```
To process more than one file, more `in` statements may be added before the call of `off cref`, or the `in` statement changed to include a list of files.

### 19.3.3 Options

Functions with the flag `nolist` will not be examined or output. Initially, all Standard Lisp functions are so flagged. (In fact, they are kept on a list `NOLIST!*`, so if you wish to see references to all functions, then `cref` should be first loaded with the command `load cref`, and this variable then set to `nil`).

It should also be remembered that any macros with the property list flag `expand`, or, if the switch `force` is on, without the property list flag `noexpand`, will be expanded before the definition is seen by the cross-reference program, so this flag can also be used to select those macros you require expanded and those you do not.

## 19.4 Prettyprinting Reduce Expressions

REDUCE includes a module for printing REDUCE syntax in a standard format. This module is activated by the switch `pret`, which is normally off.

Since the system converts algebraic input into an equivalent symbolic form, the printing program tries to interpret this as an algebraic expression before printing it. In most cases, this can be done successfully. However, there will be occasional instances where results are printed in symbolic mode form that bears little resemblance to the original input, even though it is formally equivalent.

If you want to prettyprint a whole file, say `off output,msg;MSG` and (hopefully) only clean output will result. Unlike `defn`, input is also evaluated with `pret` on.

## 19.5 Prettyprinting Standard Lisp S-Expressions

REDUCE includes a module for printing S-expressions in a standard format. The Standard Lisp function for this purpose is `prettyprint` which takes a Lisp expression and prints the formatted equivalent.

Users can also have their REDUCE input printed in this form by use of the switch `defn`. This is in fact a convenient way to convert REDUCE (or Rlisp) syntax into Lisp. `off msg;` will prevent warning messages from being printed.

NOTE: When `defn` is on, input is not evaluated.
