# 5 Statements

A statement is any combination of reserved words and expressions, and has the syntax
```Julia
R"⟨statement⟩ ::= ⟨expression⟩∣⟨proper statement⟩"
```
A REDUCE program consists of a series of commands which are statements followed by a terminator:
```
⟨terminator⟩ ::= ;∣$
```
The division of the program into lines is arbitrary. Several statements can be on one line, or one statement can be freely broken onto several lines. If the program is run interactively, statements ending with `;` or `$` are not processed until an end-of-line character is encountered. This character can vary from system to system, but is normally the Return key on an ASCII terminal. Specific systems may also use additional keys as statement terminators.

If a statement is a proper statement, the appropriate action takes place.

Depending on the nature of the proper statement some result or response may or may not be printed out, and the response may or may not depend on the terminator used.

If a statement is an expression, it is evaluated. If the terminator is a semicolon, the result is printed. If the terminator is a dollar sign, the result is not printed. Because it is not usually possible to know in advance how large an expression will be, no explicit format statements are offered to the user. However, a variety of output declarations are available so that the output can be produced in different forms. These output declarations are explained in Section 8.3.3.

The following sub-sections describe the types of proper statements in REDUCE.

```@contents
Pages = ["05-statements.md"]
```

## 5.1 Assignment Statements

These statements have the syntax
```
⟨assignment statement⟩ ::= ⟨expression⟩:=⟨expression⟩
```
The `⟨expression⟩` on the left side is normally the name of a variable, an operator symbol with its list of arguments filled in, or an array name with the proper number of integer subscript values within the array bounds. For example:
```Julia
R"a1 := b + c"
R"h(l,m) := x-2*y"     	(where h is an operator)
R"k(3,5) := x-2*y"	(where k is a 2-dim. array)
```
More general assignments such as `R"a+b := c"` are also allowed. The effect of these is explained in Section 11.2.5.

An assignment statement causes the expression on the right-hand-side to be evaluated. If the left-hand-side is a variable, the value of the right-hand-side is assigned to that unevaluated variable. If the left-hand-side is an operator or array expression, the arguments of that operator or array are evaluated, but no other simplification done. The evaluated right-hand-side is then assigned to the resulting expression. For example, if `a` is a single-dimensional array, `R"a(1+1) := b"` assigns the value `b` to the array element `a(2)`.

If a semicolon is used as the terminator when an assignment is issued as a command (i.e. not as a part of a group statement or procedure or other similar construct), the left-hand side symbol of the assignment statement is printed out, followed by a “`:=`”, followed by the value of the expression on the right.

It is also possible to write a multiple assignment statement:
```
⟨expression⟩:=…:=⟨expression⟩:=⟨expression⟩
```
In this form, each `⟨expression⟩` but the last is set to the value of the last `⟨expression⟩`. If a semicolon is used as a terminator, each expression except the last is printed followed by a “`:=`” ending with the value of the last expression.

### 5.1.1 Set and Unset Statements

```@docs
Reduce.Algebra.set
```

```@docs
Reduce.Algebra.unset
```

## 5.2 Group Statements

The group statement is a construct used where REDUCE expects a single statement, but a series of actions needs to be performed. It is formed by enclosing one or more statements (of any kind) between the symbols `<<` and `>>`, separated by semicolons or dollar signs – it doesn’t matter which. The statements are executed one after another.

Examples will be given in the sections on `if` and other types of statements in which the `<<…>>` construct is useful.

If the last statement in the enclosed group has a value, then that is also the value of the group statement. Care must be taken not to have a semicolon or dollar sign after the last grouped statement, if the value of the group is relevant: such an extra terminator causes the group to have the value `nil` or zero.

## 5.3 Conditional Statements

Not initially supported by Reduce.jl parser, see [upstream docs](http://www.reduce-algebra.com/manual/manualse16.html) for more information.

## 5.4 FOR Statements

Not initially supported by Reduce.jl parser, see [upstream docs](http://www.reduce-algebra.com/manual/manualse17.html) for more information.

## 5.5 WHILE …DO

Not initially supported by Reduce.jl parser, see [upstream docs](http://www.reduce-algebra.com/manual/manualse18.html) for more information.

## 5.6 REPEAT …UNTIL

Not initially supported by Reduce.jl parser, see [upstream docs](http://www.reduce-algebra.com/manual/manualse19.html) for more information.

## 5.7 Compound Statements

Not initially supported by Reduce.jl parser, see [upstream docs](http://www.reduce-algebra.com/manual/manualse20.html) for more information.
