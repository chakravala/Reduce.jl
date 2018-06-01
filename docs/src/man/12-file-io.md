# 12 File Handling Commands

In many applications, it is desirable to load previously prepared REDUCE files into the system, or to write output on other files. REDUCE offers four commands for this purpose, namely, `in`, `out`, `shut`, `load`, and `load_package`. The first three operators are described here; `load` and `load_package` are discussed in Section 19.2.

```@contents
Pages = ["12-file-io.md"]
```

## 12.1 IN Command

This command takes a list of file names as argument and directs the system to input each file (that should contain REDUCE statements and commands) into the system. File names can either be an identifier or a string. The explicit format of these will be system dependent and, in many cases, site dependent. The explicit instructions for the implementation being used should therefore be consulted for further details. For example:
```
R"in f1,~ggg.rr.s~"
```
will first load file `f1`, then `ggg.rr.s`. When a semicolon is used as the terminator of the `in` statement, the statements in the file are echoed on the terminal or written on the current output file. If `$` is used as the terminator, the input is not shown. Echoing of all or part of the input file can be prevented, even if a semicolon was used, by placing an `off echo;` command in the input file.

Files to be read using `in` should end with `;end;`. Note the two semicolons! First of all, this is protection against obscure difficulties the user will have if there are, by mistake, more `begin`s than `end`s on the file. Secondly, it triggers some file control book-keeping which may improve system efficiency. If `end` is omitted, an error message `~End-of-file read~` will occur.

While a file is being loaded, the special identifier `_LINE_` is replaced by the number of the current line in the file currently being read.

## 12.2 OUT Command

This command takes a single file name as argument, and directs output to that file from then on, until another `out` changes the output file, or `shut` closes it. Output can go to only one file at a time, although many can be open. If the file has previously been used for output during the current job, and not `shut`, the new output is appended to the end of the file. Any existing file is erased before its first use for output in a job, or if it had been `shut` before the new `out`.

To output on the terminal without closing the output file, the reserved file name T (for terminal) may be used. For example, `out ofile;` will direct output to the file `ofile` and `out t;` will direct output to the user’s terminal.

The output sent to the file will be in the same form that it would have on the terminal. In particular `x^2` would appear on two lines, an `x` on the lower line and a `2` on the line above. If the purpose of the output file is to save results to be read in later, this is not an appropriate form. We first must turn off the `nat` switch that specifies that output should be in standard mathematical notation.

*Example:* To create a file `abcd` from which it will be possible to read – using `in` – the value of the expression `xyz`:
```
 off echo$      % needed if your input is from a file.  
 off nat$       % output in IN-readable form. Each expression  
                % printed will end with a $ .  
 out abcd$      % output to new file  
 linelength 72$ % for systems with fixed input line length.  
 xyz:=xyz;      % will output ~XYZ := ~ followed by the value  
                % of XYZ  
 write ~;end~$  % standard for ending files for IN  
 shut abcd$     % save ABCD, return to terminal output  
 on nat$                % restore usual output form
```

## 12.3 SHUT Command

This command takes a list of names of files that have been previously opened via an `out` statement and closes them. Most systems require this action by the user before he ends the REDUCE job (if not sooner), otherwise the output may be lost. If a file is shut and a further `out` command issued for the same file, the file is erased before the new output is written.

If it is the current output file that is shut, output will switch to the terminal. Attempts to shut files that have not been opened by `out`, or an input file, will lead to errors.
