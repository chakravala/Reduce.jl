# 6 Commands and Declarations

A command is an order to the system to do something. Some commands cause visible results (such as calling for input or output); others, usually called declarations, set options, define properties of variables, or define procedures. Commands are formally defined as a statement followed by a terminator

```Julia
⟨command⟩ 	::= ⟨statement⟩⟨terminator⟩
⟨terminator⟩ 	::= ;∣$
```

Some REDUCE commands and declarations are described in the following sub-sections.

```@contents
Pages = ["06-commands-declarations.md"]
```

## 6.1 Array Declarations

Not initially supported by Reduce.jl parser, see [upstream docs](http://www.reduce-algebra.com/manual/manualse21.html) for more information.

## 6.2 Mode Handling Declarations

The `on` and `off` declarations are available to the user for controlling various system options. Each option is represented by a switch name. `on` and `off` take a list of switch names as argument and turn them on and off respectively, e.g.,
```Julia
julia> on(:time)
```
causes the system to print a message after each command giving the elapsed CPU time since the last command, or since `time` was last turned off, or the session began. Another useful switch with interactive use is `demo`, which causes the system to pause after each command in a file (with the exception of comments) until a `<Return>` is typed on the terminal. This enables a user to set up a demonstration file and step through it command by command.

```@docs
Reduce.Algebra.on
```

```@docs
Reduce.Algebra.off
```

As with most declarations, arguments to `on` and `off` may be strung together separated by commas. For example,
```Julia
julia> off(:time,:demo)
```
will turn off both the time messages and the demonstration switch.

We note here that while most `on` and `off` commands are obeyed almost instantaneously, some trigger time-consuming actions such as reading in necessary modules from secondary storage.

A diagnostic message is printed if `on` or `off` are used with a switch that is not known to the system. For example, if you misspell `demo` and type
```Julia
julia> on(:demq)
```
you will get the message
```
ERROR: Reduce: 
***** demq not defined as switch 
```

## 6.3 END

The identifier `end` has two separate uses.

1. Its use in a `R"begin… end"` bracket has been discussed in connection with compound statements.
2. Files to be read using `IN` should end with an extra `end;` command. The reason for this is explained in the section on the `IN` command. This use of `END` does not allow an immediately preceding `end` (such as the `end` of a procedure definition), so we advise using `;end;` there.

## 6.4 BYE Command

The command `R"bye"` (or alternatively `R"quit"`) stops the execution of REDUCE, closes all open output files, and returns you to the calling program (usually the operating system). Your REDUCE session is normally destroyed.

## 6.5 SHOWTIME Command

`R"showtime"` prints the elapsed time since the last call of this command or, on its first call, since the current REDUCE session began. The time is normally given in milliseconds and gives the time as measured by a system clock. The operations covered by this measure are system dependent.

## 6.6 DEFINE Command

```@docs
Reduce.Algebra.define
```

*Example:*
```
        define be==,x=y+z;
```
means that `be` will be interpreted as an equal sign, and `x` as the expression `y+z` from then on. This renaming is done at parse time, and therefore takes precedence over any other replacement declared for the same identifier. It stays in effect until the end of the REDUCE run.

The identifiers `ALGEBRAIC` and `SYMBOLIC` have properties which prevent `define` from being used on them. To define `ALG` to be a synonym for `ALGEBRAIC`, use the more complicated construction
```
        put(’alg,’newnam,’algebraic);
```
