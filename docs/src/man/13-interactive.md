# 13 Commands for Interactive Use

REDUCE is designed as an interactive system, but naturally it can also operate in a batch processing or background mode by taking its input command by command from the relevant input stream. There is a basic difference, however, between interactive and batch use of the system. In the former case, whenever the system discovers an ambiguity at some point in a calculation, such as a forgotten type assignment for instance, it asks the user for the correct interpretation. In batch operation, it is not practical to terminate the calculation at such points and require resubmission of the job, so the system makes the most obvious guess of the user’s intentions and continues the calculation.

There is also a difference in the handling of errors. In the former case, the computation can continue since the user has the opportunity to correct the mistake. In batch mode, the error may lead to consequent erroneous (and possibly time consuming) computations. So in the default case, no further evaluation occurs, although the remainder of the input is checked for syntax errors. A message `~Continuing with parsing only~` informs the user that this is happening. On the other hand, the switch `errcont`, if on, will cause the system to continue evaluating expressions after such errors occur.

When a syntactical error occurs, the place where the system detected the error is marked with three dollar signs (`$$$`). In interactive mode, the user can then use `ed` to correct the error, or retype the command. When a non-syntactical error occurs in interactive mode, the command being evaluated at the time the last error occurred is saved, and may later be reevaluated by the command `retry`.

```@contents
Pages = ["13-interactive.md"]
```

## 13.1 Referencing Previous Results

It is often useful to be able to reference results of previous computations during a REDUCE session. For this purpose, REDUCE maintains a history of all interactive inputs and the results of all interactive computations during a given session. These results are referenced by the command number that REDUCE prints automatically in interactive mode. To use an input expression in a new computation, one writes `input(n)`, where `n` is the command number. To use an output expression, one writes `ws(n)`. `ws` references the previous command. E.g., if command number 1 was `int(x-1,x)` and the result of command number 7 was `x-1`, then
```
        2*input(1)-ws(7)^2;
```
would give the result `-1`, whereas
```
        2*ws(1)-ws(7)^2;
```
would yield the same result, but *without* a recomputation of the integral.

The operator `display` is available to display previous inputs. If its argument is a positive integer, n say, then the previous n inputs are displayed. If its argument is `all` (or in fact any non-numerical expression), then all previous inputs are displayed.

## 13.2 Interactive Editing

Not initially supported by Reduce.jl parser, see [upstream docs](http://www.reduce-algebra.com/manual/manualse77.html) for more information.

## 13.3 Interactive File Control

If input is coming from an external file, the system treats it as a batch processed calculation. If the user desires interactive response in this case, he can include the command `on int;` in the file. Likewise, he can issue the command `off int;` in the main program if he does not desire continual questioning from the system. Regardless of the setting of `int`, input commands from a file are not kept in the system, and so cannot be edited using `ed`. However, many implementations of REDUCE provide a link to an external system editor that can be used for such editing. The specific instructions for the particular implementation should be consulted for information on this.

Two commands are available in REDUCE for interactive use of files. `pause;` may be inserted at any point in an input file. When this command is encountered on input, the system prints the message `CONT?` on the user’s terminal and halts. If the user responds `Y` (for yes), the calculation continues from that point in the file. If the user responds `N` (for no), control is returned to the terminal, and the user can input further statements and commands. Later on he can use the command `cont;` to transfer control back to the point in the file following the last `pause` encountered. A top-level `pause;` from the user’s terminal has no effect.
