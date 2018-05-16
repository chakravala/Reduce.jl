# 3 Expressions

REDUCE expressions may be of several types and consist of sequences of numbers, variables, operators, left and right parentheses and commas. The most common types are as follows:

```@contents
Pages = ["03-expressions.md"]
```

Reduce expressions encapsulated into `RExpr` objects can be manipulated within julia using the standard syntax. Create an expression object either using the `RExpr("expression")` string constructor or `R"expression"`. Additionally, arbitrary julia expressions can also be parsed directly using the `RExpr(expr)` constructor. Internally `RExpr` objects are represented as an array that can be accessed by calling `*.str[n]` on the object.
Julia abstract syntax trees are automatically converted into sequences of reduce statements (using `RExpr` constructor).

## 3.1 Scalar Expressions

Using the arithmetic operations `+`, `-`, `*`, `/`, `^` (power) and parentheses, scalar expressions are composed from numbers, ordinary “scalar” variables (identifiers), array names with subscripts, operator or procedure names with arguments and statement expressions.

*Examples:*
```Julia
RExpr("x")
R"x^3 - 2*y/(2*z^2 - df(x,z))"
R"(p^2 + m^2)^(1/2)*log (y/m)"
R"a(5) + b(i,q)"
```
The symbol `**` may be used as an alternative to the caret symbol (`^`) for forming powers, particularly in those systems that do not support a caret symbol.

Statement expressions, usually in parentheses, can also form part of a scalar expression, as in the example
```Julia
R"w + (c:=x+y) + z"
```
When the algebraic value of an expression is needed, REDUCE determines it, starting with the algebraic values of the parts, roughly as follows:

Variables and operator symbols with an argument list have the algebraic values they were last assigned, or if never assigned stand for themselves. However, array elements have the algebraic values they were last assigned, or, if never assigned, are taken to be 0.

Procedures are evaluated with the values of their actual parameters.

In evaluating expressions, the standard rules of algebra are applied. Unfortunately, this algebraic evaluation of an expression is not as unambiguous as is numerical evaluation. This process is generally referred to as “simplification” in the sense that the evaluation usually but not always produces a simplified form for the expression.

There are many options available to the user for carrying out such simplification. If the user doesn’t specify any method, the default method is used. The default evaluation of an expression involves expansion of the expression and collection of like terms, ordering of the terms, evaluation of derivatives and other functions and substitution for any expressions which have values assigned or declared (see assignments and `let` statements). In many cases, this is all that the user needs.

The declarations by which the user can exercise some control over the way in which the evaluation is performed are explained in other sections. For example, if a real (floating point) number is encountered during evaluation, the system will normally convert it into a ratio of two integers. If the user wants to use real arithmetic, he can effect this by the command `rounded(true)`. Other modes for coefficient arithmetic are described elsewhere.

If an illegal action occurs during evaluation (such as division by zero) or functions are called with the wrong number of arguments, and so on, an appropriate error message is generated.

## 3.2 Integer Expressions

These are expressions which, because of the values of the constants and variables in them, evaluate to whole numbers.

*Examples:*
```Julia
R"2";      R"37 * 999";       R"(x + 3)^2 - x^2 - 6*x"
```
are obviously integer expressions.
```Julia
R"j + k - 2 * j^2"
```
is an integer expression when `J` and `K` have values that are integers, or if not integers are such that “the variables and fractions cancel out”, as in
```Julia
R"k - 7/3 - j + 2/3 + 2*j^2"
```

## 3.3 Boolean Expressions

Not initially supported by Reduce.jl parser, see [upstream docs](http://www.reduce-algebra.com/manual/manualse10.html) for more information.

## 3.4 Equations

Equations are a particular type of expression with the syntax
```
R"⟨expression⟩=⟨expression⟩"
```
In addition to their role as boolean expressions, they can also be used as arguments to several operators (e.g., `solve`), and can be returned as values.

Under normal circumstances, the right-hand-side of the equation is evaluated but not the left-hand-side. This also applies to any substitutions made by the `sub` operator. If both sides are to be evaluated, the switch `evallhseqp` should be turned on.

To facilitate the handling of equations, two selectors, `lhs` and `rhs`, which return the left- and right-hand sides of an equation respectively, are provided.

```@docs
Reduce.Algebra.lhs
```

```@docs
Reduce.Algebra.rhs
```

## 3.5 Proper Statements as Expressions

Several kinds of proper statements deliver an algebraic or numerical result of some kind, which can in turn be used as an expression or part of an expression. For example, an assignment statement itself has a value, namely the value assigned. So
```Julia
R"2 * (x := a+b)"
```
is equal to `R"2*(a+b)"`, as well as having the “side-effect” of assigning the value `a+b` to `X`. In context,
```Julia
R"y := 2 * (x := a+b);"
```
sets `X` to `a+b` and `Y` to `2*(a+b)`.

**Note that if the Reduce.jl parser is used to convert these types of expressions to Julia AST, issues can occur since an equivalent feature does not exist in the Julia language.**

The sections on the various proper statement types indicate which of these statements are also useful as expressions.
