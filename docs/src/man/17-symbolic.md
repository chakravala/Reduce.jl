# 17 Symbolic Mode

At the system level, REDUCE is based on a version of the programming language Lisp known as Standard Lisp which is described in J. Marti, Hearn, A. C., Griss, M. L. and Griss, C., “Standard LISP Report" SIGPLAN Notices, ACM, New York, 14, No 10 (1979) 48-68. We shall assume in this section that the reader is familiar with the material in that paper. This also assumes implicitly that the reader has a reasonable knowledge about Lisp in general, say at the level of the LISP 1.5 Programmer’s Manual (McCarthy, J., Abrahams, P. W., Edwards, D. J., Hart, T. P. and Levin, M. I., “LISP 1.5 Programmer’s Manual”, M.I.T. Press, 1965) or any of the books mentioned at the end of this section. Persons unfamiliar with this material will have some difficulty understanding this section.

Although REDUCE is designed primarily for algebraic calculations, its source language is general enough to allow for a full range of Lisp-like symbolic calculations. To achieve this generality, however, it is necessary to provide the user with two modes of evaluation, namely an algebraic mode and a symbolic mode. To enter symbolic mode, the user types `symbolic;` (or `lisp;`) and to return to algebraic mode one types `algebraic;`. Evaluations proceed differently in each mode so the user is advised to check what mode he is in if a puzzling error arises. He can find his mode by typing
```
        eval_mode;
```
The current mode will then be printed as `ALGEBRAIC` or `SYMBOLIC`.

Expression evaluation may proceed in either mode at any level of a calculation, provided the results are passed from mode to mode in a compatible manner. One simply prefixes the relevant expression by the appropriate mode. If the mode name prefixes an expression at the top level, it will then be handled as if the global system mode had been changed for the scope of that particular calculation.

For example, if the current mode is `algebraic`, then the commands
```
        symbolic car ’(a);  
        x+y;
```
will cause the first expression to be evaluated and printed in symbolic mode and the second in algebraic mode. Only the second evaluation will thus affect the expression workspace. On the other hand, the statement
```
        x + symbolic car ’(12);
```
will result in the algebraic value `x+12`.

The use of `symbolic` (and equivalently `algebraic`) in this manner is the same as any operator. That means that parentheses could be omitted in the above examples since the meaning is obvious. In other cases, parentheses must be used, as in
```
        symbolic(x := ’a);
```
Omitting the parentheses, as in
```
        symbolic x := a;
```
would be wrong, since it would parse as
```
        symbolic(x) := a;
```
For convenience, it is assumed that any operator whose *first* argument is quoted is being evaluated in symbolic mode, regardless of the mode in effect at that time. Thus, the first example above could be equally well written:
```
        car ’(a);
```
Except where explicit limitations have been made, most REDUCE algebraic constructions carry over into symbolic mode. However, there are some differences. First, expression evaluation now becomes Lisp evaluation. Secondly, assignment statements are handled differently, as we shall discuss shortly. Thirdly, local variables and array elements are initialized to `nil` rather than `0`. (In fact, any variables not explicitly declared `INTEGER` are also initialized to `nil` in algebraic mode, but the algebraic evaluator recognizes `nil` as `0`.) Finally, function definitions follow the conventions of Standard Lisp.

To begin with, we mention a few extensions to our basic syntax which are designed primarily if not exclusively for symbolic mode.

```@contents
Pages = ["17-symbolic.md"]
```

## 17.1 Symbolic Infix Operators

There are three binary infix operators in REDUCE intended for use in symbolic mode, namely `.` (`cons`), `eq` and `memq`. The precedence of these operators was given in another section.

## 17.2 Symbolic Expressions

These consist of scalar variables and operators and follow the normal rules of the Lisp meta language.

*Examples:*
```
        x  
        car u . reverse v  
        simp (u+v^2)
```

## 17.3 Quoted Expressions

Because symbolic evaluation requires that each variable or expression has a value, it is necessary to add to REDUCE the concept of a quoted expression by analogy with the Lisp `quote` function. This is provided by the single quote mark `’`. For example,
```
’a    		represents the Lisp S-expression	(quote a)
’(a b c)   	represents the Lisp S-expression	(quote (a b c))
```
Note, however, that strings are constants and therefore evaluate to themselves in symbolic mode. Thus, to print the string `~A String~`, one would write
```
        prin2 ~A String~;
```
Within a quoted expression, identifier syntax rules are those of REDUCE. Thus `(A !. B)` is the list consisting of the three elements `A`, `.`, and `B`, whereas `(A . B)` is the dotted pair of `A` and `B`.

## 17.4 Lambda Expressions

`lambda` expressions provide the means for constructing Lisp `lambda` expressions in symbolic mode. They may not be used in algebraic mode.

Syntax:
```
⟨LAMBDAexpression⟩ ::=  LAMBDA ⟨varlist⟩⟨terminator⟩⟨statement⟩
```
where
```
⟨varlist⟩ ::= (⟨variable⟩,…,⟨variable⟩)
```
e.g.,
```
        lambda (x,y); car x . cdr y;
```
is equivalent to the Lisp `lambda` expression
```
        (lambda (x y) (cons (car x) (cdr y)))
```
The parentheses may be omitted in specifying the variable list if desired.

`lambda` expressions may be used in symbolic mode in place of prefix operators, or as an argument of the reserved word `function`.

In those cases where a `lambda` expression is used to introduce local variables to avoid recomputation, a `where` statement can also be used. For example, the expression
```
        (lambda (x,y); list(car x,cdr x,car y,cdr y))  
            (reverse u,reverse v)
```
can also be written
```
      {car x,cdr x,car y,cdr y} where x=reverse u,y=reverse v
```
Where possible, `where` syntax is preferred to `lambda` syntax, since it is more natural.

## 17.5 Symbolic Assignment Statements

In symbolic mode, if the left side of an assignment statement is a variable, a `setq` of the right-hand side to that variable occurs. If the left-hand side is an expression, it must be of the form of an array element, otherwise an error will result. For example, `x:=y` translates into `(SETQ X Y)` whereas `a(3) := 3` will be valid if `a` has been previously declared a single dimensioned array of at least four elements.

## 17.6 FOR EACH Statement

The `for each` form of the `for` statement, designed for iteration down a list, is more general in symbolic mode. Its syntax is:
```
        FOR EACH ID:identifier {IN|ON} LST:list  
            {DO|COLLECT|JOIN|PRODUCT|SUM} EXPRN:S-expr
```
As in algebraic mode, if the keyword `in` is used, iteration is on each element of the list. With `on`, iteration is on the whole list remaining at each point in the iteration. As a result, we have the following equivalence between each form of `for each` and the various mapping functions in Lisp:
```
	DO	COLLECT	JOIN
IN	MAPC	MAPCAR	MAPCAN
ON	MAP	MAPLIST	MAPCON
```
*Example:* To list each element of the list `(a b c)`:
```
        for each x in ’(a b c) collect list x;
```

## 17.7 Symbolic Procedures

All the functions described in the Standard Lisp Report are available to users in symbolic mode. Additional functions may also be defined as symbolic procedures. For example, to define the Lisp function ASSOC, the following could be used:
```
        symbolic procedure assoc(u,v);  
           if null v then nil  
            else if u = caar v then car v  
            else assoc(u, cdr v);
```
If the default mode were symbolic, then `symbolic` could be omitted in the above definition. `macro`s may be defined by prefixing the keyword `procedure` by the word 	`macro`. (In fact, ordinary functions may be defined with the keyword `expr` prefixing `procedure` as was used in the Standard Lisp Report.) For example, we could define a `macro conscons` by
```
        symbolic macro procedure conscons l;  
           expand(cdr l,’cons);
```
Another form of macro, the `smacro` is also available. These are described in the Standard Lisp Report. The Report also defines a function type `fexpr`. However, its use is discouraged since it is hard to implement efficiently, and most uses can be replaced by macros. At the present time, there are no `fexpr`s in the core REDUCE system.

## 17.8 Standard Lisp Equivalent of Reduce Input

A user can obtain the Standard Lisp equivalent of his REDUCE input by turning on the switch `defn` (for definition). The system then prints the Lisp translation of his input but does not evaluate it. Normal operation is resumed when `defn` is turned off.

## 17.9 Communicating with Algebraic Mode

Not initially supported by Reduce.jl parser, see [upstream docs](http://www.reduce-algebra.com/manual/manualse177.html) for more information.

## 17.10 Rlisp ’88

Rlisp ’88 is a superset of the Rlisp that has been traditionally used for the support of REDUCE. It is fully documented in the book Marti, J.B., “RLISP ’88: An Evolutionary Approach to Program Design and Reuse”, World Scientific, Singapore (1993). Rlisp ’88 adds to the traditional Rlisp the following facilities:

1. more general versions of the looping constructs `for`, `repeat` and `while`;
2. support for a backquote construct;
3. support for active comments;
4. support for vectors of the form name[index];
5. support for simple structures;
6. support for records.

In addition, “`-`” is a letter in Rlisp ’88. In other words, `A-B` is an identifier, not the difference of the identifiers `A` and `B`. If the latter construct is required, it is necessary to put spaces around the - character. For compatibility between the two versions of Rlisp, we recommend this convention be used in all symbolic mode programs.

To use Rlisp ’88, type `on rlisp88;`. This switches to symbolic mode with the Rlisp ’88 syntax and extensions. While in this environment, it is impossible to switch to algebraic mode, or prefix expressions by “algebraic”. However, symbolic mode programs written in Rlisp ’88 may be run in algebraic mode provided the rlisp88 package has been loaded. We also expect that many of the extensions defined in Rlisp ’88 will migrate to the basic Rlisp over time. To return to traditional Rlisp or to switch to algebraic mode, say “`off rlisp88;`”.

## 17.11 References

There are a number of useful books which can give you further information about LISP. Here is a selection:

Allen, J.R., “The Anatomy of LISP”, McGraw Hill, New York, 1978.

McCarthy J., P.W. Abrahams, J. Edwards, T.P. Hart and M.I. Levin, “LISP 1.5 Programmer’s Manual”, M.I.T. Press, 1965.

Touretzky, D.S, “LISP: A Gentle Introduction to Symbolic Computation”, Harper & Row, New York, 1984.

Winston, P.H. and Horn, B.K.P., “LISP”, Addison-Wesley, 1981.
