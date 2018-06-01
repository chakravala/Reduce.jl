# 7 Built-in Prefix Operators

In the following subsections are descriptions of the most useful prefix operators built into REDUCE that are not defined in other sections (such as substitution operators). Some are fully defined internally as procedures; others are more nearly abstract operators, with only some of their properties known to the system.

In many cases, an operator is described by a prototypical header line as follows. Each formal parameter is given a name and followed by its allowed type. The names of classes referred to in the definition are printed in lower case, and parameter names in upper case. If a parameter type is not commonly used, it may be a specific set enclosed in brackets `{ … }`. Operators that accept formal parameter lists of arbitrary length have the parameter and type class enclosed in square brackets indicating that zero or more occurrences of that argument are permitted. Optional parameters and their type classes are enclosed in angle brackets.

```@contents
Pages = ["07-prefix-ops.md"]
```

## 7.1 Numerical Operators

REDUCE includes a number of functions that are analogs of those found in most numerical systems. With numerical arguments, such functions return the expected result. However, they may also be called with non-numerical arguments. In such cases, except where noted, the system attempts to simplify the expression as far as it can. In such cases, a residual expression involving the original operator usually remains. These operators are as follows:

```@docs
Reduce.Algebra.abs
```

```@docs
Reduce.Algebra.ceiling
```

```@docs
Reduce.Algebra.conj
```

```@docs
Reduce.Algebra.factorial
```

```@docs
Reduce.Algebra.fix
```

```@docs
Reduce.Algebra.floor
```

```@docs
Reduce.Algebra.impart
```

```@docs
Reduce.Algebra.max
```

```@docs
Reduce.Algebra.min
```

```@docs
Reduce.Algebra.nextprime
```

```@docs
Reduce.Algebra.random
```

```@docs
Reduce.Algebra.random_new_seed
```

```@docs
Reduce.Algebra.repart
```

```@docs
Reduce.Algebra.round
```

```@docs
Reduce.Algebra.sign
```

## 7.2 Mathematical Functions

REDUCE knows that the following represent mathematical functions that can take arbitrary scalar expressions as their argument(s):
```
acos acosh acot acoth acsc acsch asec asech asin  
asinh atan atanh atan2 beta ci cos cosh cot coth csc  
csch dilog ei exp gamma hypot ibeta igamma ln log  
logb log10 sec sech si sin sinh sqrt tan tanh  
airy_ai airy_aiprime airy_bi airy_biprime  
besseli besselj besselk bessely  
hankel1 hankel2 kummerm kummeru lommel1 lommel2  
struveh struvel whittakerm whittakeru  
polygamma psi zeta  
solidharmonicy sphericalharmonicy
```
where `log` is the natural logarithm (and equivalent to `ln`), and `logb` has two arguments of which the second is the logarithmic base.

The derivatives of all these functions are also known to the system.

REDUCE knows various elementary identities and properties of these functions. For example:

>      cos(-x) = cos(x)              sin(-x) = - sin (x)  
>      cos(n*pi) = (-1)^n            sin(n*pi) = 0  
>      log(e)  = 1                   e^(i*pi/2) = i  
>      log(1)  = 0                   e^(i*pi) = -1  
>      log(e^x) = x                  e^(3*i*pi/2) = -i

Beside these identities, there are a lot of simplifications for elementary functions defined in the REDUCE system as rulelists. In order to view these, the `showrules` operator can be used, e.g.
```Julia
reduce> showrules tan;  
 
{tan(~n*arbint(~i)*pi + ~(~ x)) => tan(x) when fixp(n),  
 
 tan(~x)  
 
  => trigquot(sin(x),cos(x)) when knowledge_about(sin,x,tan),  
 
      ~x + ~(~ k)*pi  
 tan(----------------)  
            ~d  
 
             x                  k                     k     1  
  =>  - cot(---) + i*pi*impart(---)) when abs(repart(---))=---,  
             d                  d                     d     2  
 
      ~(~ w) + ~(~ k)*pi           w      k                k  
 tan(--------------------) => tan(--- + (--- - fix(repart(---)))*pi)  
            ~(~ d)                 d      d                d  
 
                              k  
 when whereexp({rp => repart(---)},bool-eval(ratnump(rp) and abs(rp)>=1)),  
                              d  
 
 tan(atan(~x)) => x,  
 
                             2  
 df(tan(~x),~x) => 1 + tan(x) }  

```

For further simplification, especially of expressions involving trigonometric functions, see the TRIGSIMP package (chapter 16.72) documentation.

Functions not listed above may be defined in the special functions package SPECFN.

The user can add further rules for the reduction of expressions involving these operators by using the `let` command.

In many cases it is desirable to expand product arguments of logarithms, or collect a sum of logarithms into a single logarithm. Since these are inverse operations, it is not possible to provide rules for doing both at the same time and preserve the REDUCE concept of idempotent evaluation. As an alternative, REDUCE provides two switches `expandlogs` and `combinelogs` to carry out these operations. Both are off by default, and are subject to the value of the switch `precise`. This switch is on by default and prevents modifications that may be false in a complex domain. Thus to expand `log(3*y)` into a sum of logs, one can say
```Julia
julia> Algebra.on(:expandlogs);

julia> Algebra.log(:(3*y))
```
whereas to expand `log(x*y)` into a sum of logs, one needs to say
```Julia
julia> Algebra.off(:precise); Algebra.on(:expandlogs);

julia> Algebra.log(:(x*y))
```
To combine this sum into a single log:
```Julia
julia> Algebra.off(:precise); Algebra.on(:combinelogs);

julia> Alebra.:+(log(:x),log(:y))
```
These switches affect the logarithmic functions `log10` (base 10) and `logb` (arbitrary base) as well.

At the present time, it is possible to have both switches on at once, which could lead to infinite recursion. However, an expression is switched from one form to the other in this case. Users should not rely on this behavior, since it may change in the next release.

The current version of REDUCE does a poor job of simplifying surds. In particular, expressions involving the product of variables raised to non-integer powers do not usually have their powers combined internally, even though they are printed as if those powers were combined. For example, the expression
```Julia
reduce> x^(1/3)*x^(1/6)
```
will print as
```
sqrt(x)
```
but will have an internal form containing the two exponentiated terms. If you now subtract `sqrt(x)` from this expression, you will not get zero. Instead, the confusing form
```
sqrt(x) - sqrt(x)
```
will result. To combine such exponentiated terms, the switch `combineexpt` should be turned on.

The square root function can be input using the name `sqrt`, or the power operation `^(1/2)`. On output, unsimplified square roots are normally represented by the operator `sqrt` rather than a fractional power. With the default system switch settings, the argument of a square root is first simplified, and any divisors of the expression that are perfect squares taken outside the square root argument. The remaining expression is left under the square root. Thus the expression
```Julia
julia> Algebra.sqrt(:(-8a^2*b))
```
becomes
```Julia
:(2 * sqrt(b) * sqrt(2) * a * im)
```
Note that such simplifications can cause trouble if A is eventually given a value that is a negative number. If it is important that the positive property of the square root and higher even roots always be preserved, the switch PRECISE should be set on (the default value). This causes any non-numerical factors taken out of surds to be represented by their absolute value form. With PRECISE on then, the above example would become
```Julia
:(2 * sqrt(-2b) * abs(a))
```
However, this is incorrect in the complex domain, where the ``\sqrt{x^2}`` is not identical to ``|x|``. To avoid the above simplification, the switch `precise_complex` should be set on (default is off). For example:
```Julia
julia> Algebra.on(:precise_complex); Algebra.sqrt(:(-8a^2*b))
```
yields the output
```Julia
:(2 * sqrt(-2 * a ^ 2 * b))
```
The statement that REDUCE knows very little about these functions applies only in the mathematically exact off rounded mode. If `rounded` is on, any of the functions
```
acos acosh acot acoth acsc acsch asec asech asin  
asinh atan atanh atan2 cos cosh cot coth csc csch  
exp hypot ibeta igamma ln log logb log10 psi sec  
sech sin sinh sqrt tan tanh
```
when given a numerical argument has its value calculated to the current degree of floating point precision. In addition, real (non-integer valued) powers of numbers will also be evaluated.

If the `complex` switch is turned on in addition to `rounded`, these functions will also calculate a real or complex result, again to the current degree of floating point precision, if given complex arguments. For example,
```Julia
julia> @rounded @complex 2.3^(5.6im)
:(-0.0480793490914 - 0.998843519372im)

julia> @rounded @complex cos(2+3im)
:(-4.18962569097 - 9.10922789376im)
```

## 7.3 Bernoulli Numbers and Euler Numbers

```@docs
Reduce.Algebra.bernoulli
```

Euler numbers are computed by the unary operator Euler, which return the nth Euler number. The computation is derived directly from Pascal’s triangle of binomial coefficients.

## 7.4 Fibonacci Numbers and Fibonacci Polynomials

```@docs
Reduce.Algebra.fibonacci
```

```@docs
Reduce.Algebra.fibonaccip
```

## 7.5 Motzkin numbers

```@docs
Reduce.Algebra.motzkin
```

## 7.6 CHANGEVAR Operator

Author: G. Üçoluk.

```@docs
Reduce.Algebra.changevar
```

The switch `dispjacobian` governs the display the entries of the inverse Jacobian, it is `off` per default.

The mathematics behind the change of independent variable(s) in differential equations is quite straightforward. It is basically the application of the chain rule. If the dependent variable of the differential equation is ``F`` , the independent variables are ``x_i`` and the new independent variables are ``u_i`` (where ``i=1…n``) then the first derivatives are:
``
\frac{-\partial F}{\partial x_i} =  \frac{\partial F}{\partial u_j} \frac{\partial u_j}{\partial x_i}
``

We assumed Einstein’s summation convention. Here the problem is to calculate the ``∂u_j∕∂x_i`` terms if the change of variables is given by
``
x_i = f_i (u_1 ,...,u_n  ).
``

The first thought might be solving the above given equations for ``u_j`` and then differentiating them with respect to ``x_i``, then again making use of the equations above, substituting new variables for the old ones in the calculated derivatives. This is not always a preferable way to proceed. Mainly because the functions ``f_i`` may not always be easily invertible. Another approach that makes use of the Jacobian is better. Consider the above given equations which relate the old variables to the new ones. Let us differentiate them:

```math
\frac{\partial x_j}{\partial x_i} = \frac{\partial f_j}{\partial x_i}
```

```math
\delta_{ij} = \frac{\partial f_j \partial u_k}{\partial u_k \partial x_i}
```

The first derivative is nothing but the ``(j,k)`` th entry of the Jacobian matrix.

So if we speak in matrix language
``
1 = J ⋅ D
``
where we defined the Jacobian
``
J_{ij} = \frac{\partial f_i}{\partial u_j}
``
and the matrix of the derivatives we wanted to obtain as
``
D_{ij} = \frac{\partial u_i}{\partial x_j}.
``
If the Jacobian has a non-vanishing determinant then it is invertible and we are able to write from the matrix equation above:
``
D =  J^{-1}
``
so finally we have what we want
``
\frac{\partial u_i}{\partial x_j} = [ J^{-1} ]_{ij}
``

The higher derivatives are obtained by the successive application of the chain rule and using the definitions of the old variables in terms of the new ones. It can be easily verified that the only derivatives that are needed to be calculated are the first order ones which are obtained above.

### 7.6.1 CHANGEVAR example: The 2-dim. Laplace Equation

The 2-dimensional Laplace equation in cartesian coordinates is:
``
\frac{\partial^2 u}{\partial x^2} + \frac{\partial^2 u}{\partial y^2} = 0
``
Now assume we want to obtain the polar coordinate form of Laplace equation. The change of variables is:
``
x = r\cos θ,    y = r\sin θ
``
The solution using `changevar` is as follows
```Julia
Algebra.changevar((:u,),(:r,:θ),(:(x=r*cos(θ)),:(y=r*sin(θ))),
            (:(df(u(x,y),x,2)+df(u(x,y),y,2)),) )
```
Here we could omit the list parenthesis in the first and last arguments (because those lists have only one member) and the list parenthesis in the third argument (because they are optional), but you cannot leave off the list parenthesis in the second argument. So one could equivalently write
```Julia
Algebra.changevar(:u,(:r,:θ),:(x==r*cos(θ)),:(y==r*sin(θ)),  
             :(df(u(x,y),x,2)+df(u(x,y),y,2)) )
```
If you have tried out the above example, you will notice that the denominator contains a ``\cos^2θ + \sin^2θ`` which is actually equal to 1. This has of course nothing to do with `changevar`. One has to be overcome these pattern matching problems by the conventional methods REDUCE provides (a rule, for example, will fix it).

Secondly you will notice that your `u(x,y)` operator has changed to `u(r,θ)` in the result. Nothing magical about this. That is just what we do with pencil and paper. `u(r,θ)` represents the the transformed dependent variable.

### 7.6.2 Another CHANGEVAR example: An Euler Equation

Consider a differential equation which is of Euler type, for instance:
``
x^3y ′′′ - 3x^2y′′ + 6xy ′ - 6y = 0
``
where prime denotes differentiation with respect to ``x``. As is well known, Euler type of equations are solved by a change of variable:
``
x = e^u.
``
So our call to `changevar` reads as follows:
```Julia
Algebra.changevar(:y, :u, :(x==e^u), :(x^3*df(y(x),x,3)-  
             3*x^2*df(y(x),x,2)+6*x*df(y(x),x)-6*y(x)))
```
and returns the result
```Julia
:(((11 * df(y(u), u) - 6 * y(u)) - 6 * df(y(u), u, 2)) + df(y(u), u, 3))
```

## 7.7 CONTINUED_FRACTION Operator

Not initially supported by Reduce.jl parser, see [upstream docs](http://www.reduce-algebra.com/manual/manualse33.html) for more information.

## 7.8 DF Operator

```@docs
Reduce.Algebra.df
```

### 7.8.1 Switches influencing differentiation

Consider `df(u,x,y,z)`. If none of `x`,`y`,`z` are equal to `u` then the order of differentiation is commuted into a canonical form, unless the switch `nocommutedf` is turned on (default is off). if at least one of `x`,`y`,`z` is equal to `u` then the order of differentiation is not commuted and the derivative is not simplified to zero, unless the switch `commutedf` is turned on. It is off by default.

If `commutedf` is off and the switch `simpnoncomdf` is on then simplify as follows:
```
df(u,x,u)        ->  df(u,x,2) / df(u,x)  
df(u,x,n,u)      ->  df(u,x,n+1) / df(u,x)
```
provided `u` depends only on the one variable `x`. This simplification removes the non-commutative aspect of the derivative.

If the switch `expanddf` is turned on then REDUCE uses the chain rule to expand symbolic derivatives of indirectly dependent variables provided the result is unambiguous, i.e. provided there is no direct dependence. It is off by default. Thus, for example, given
```Julia
julia> Algebra.depend(:f,:u,:v); Algebra.depend((:u,:v),:x)

julia> Algebra.on(:expanddf)

julia> Algebra.df(:f,:x)
:(df(f, u) * df(u, x) + df(f, v) * df(v, x))
```
whereas after
```Julia
julia> Algebra.depend(:f,:x)
```
`df(:f,:x)` does not expand at all (since the result would be ambiguous and the algorithm would loop).

Turning on the switch `allowdfint` allows “differentiation under the integral sign”, i.e.
```
df(int(y, x), v) -> int(df(y, v), x)
```
if this results in a simplification. If the switch `dfint` is also turned on then this happens regardless of whether the result simplifies. Both switches are off by default.

### 7.8.2 Adding Differentiation Rules

The `let` statement can be used to introduce rules for differentiation of user-defined operators. Its general form is
```Julia
R"for all ⟨var1⟩,…,⟨varn⟩ let df(⟨operator⟩⟨varlist⟩,⟨vari⟩)=⟨expression⟩"
```
where `⟨varlist⟩ ::= (⟨var1⟩,…,⟨varn⟩)`, and `⟨var1⟩,…,⟨varn⟩` are the dummy variable arguments of `⟨operator⟩`.

An analogous form applies to infix operators.

*Examples:*
```Julia
R"for all x let df(tan x,x)= 1 + tan(x)^2"
```
(This is how the tan differentiation rule appears in the REDUCE source.)
```Julia
R"for all x,y let df(f(x,y),x)=2*f(x,y),  df(f(x,y),y)=x*f(x,y)"
```
Notice that all dummy arguments of the relevant operator must be declared arbitrary by the `for all` command, and that rules may be supplied for operators with any number of arguments. If no differentiation rule appears for an argument in an operator, the differentiation routines will return as result an expression in terms of `df`. For example, if the rule for the differentiation with respect to the second argument of `f` is not supplied, the evaluation of `df(f(x,z),z)` would leave this expression unchanged. (No `depend` declaration is needed here, since `f(x,z)` obviously “depends on” `z`.)

Once such a rule has been defined for a given operator, any future differentiation rules for that operator must be defined with the same number of arguments for that operator, otherwise we get the error message
```Julia
ERROR: Reduce: 
Incompatible df rule argument length for <operator>
```

## 7.9 INT Operator

```@docs
Reduce.Algebra.int
```

### 7.9.1 Options

The switch `trint` when on will trace the operation of the algorithm. It produces a great deal of output in a somewhat illegible form, and is not of much interest to the general user. It is normally off.

The switch `trintsubst` when on will trace the heuristic attempts to solve the integral by substitution. It is normally off.

If the switch `failhard` is on the algorithm will terminate with an error if the integral cannot be done in closed terms, rather than return a formal integration form. `failhard` is normally off.

The switch `nolnr` suppresses the use of the linear properties of integration in cases when the integral cannot be found in closed terms. It is normally off.

The switch `nointsubst` disables the heuristic attempts to solve the integral by substitution. It is normally off.

### 7.9.2 Advanced Use

If a function appears in the integrand that is not one of the functions `exp`, `erf`, `tan`, `atan`, `log`, `dilog` then the algorithm will make an attempt to integrate the argument if it can, differentiate it and reach a known function. However the answer cannot be guaranteed in this case. If a function is known to be algebraically independent of this set it can be flagged transcendental by
```Julia
R"flag(’(trilog),’transcendental)"
```
in which case this function will be added to the permitted field descriptors for a genuine decision procedure. If this is done the user is responsible for the mathematical correctness of his actions.

The standard version does not deal with algebraic extensions. Thus integration of expressions involving square roots and other like things can lead to trouble. A contributed package that supports integration of functions involving square roots is available, however (ALGINT, chapter 16.1). In addition there is a definite integration package, DEFINT( chapter 16.18).

### 7.9.3 References

A. C. Norman & P. M. A. Moore, “Implementing the New Risch Algorithm”, Proc. 4th International Symposium on Advanced Comp. Methods in Theor. Phys., CNRS, Marseilles, 1977.

S. J. Harrington, “A New Symbolic Integration System in Reduce”, Comp. Journ. 22 (1979) 2.

A. C. Norman & J. H. Davenport, “Symbolic Integration — The Dust Settles?”, Proc. EUROSAM 79, Lecture Notes in Computer Science 72, Springer-Verlag, Berlin Heidelberg New York (1979) 398-407.

## 7.10 LENGTH Operator

```@docs
Reduce.Algebra.length
```

## 7.11 MAP Operator

```@docs
Reduce.Algebra.map
```

## 7.12 MKID Operator

```@docs
Reduce.Algebra.mkid
```

The `set` statement can be used to give a value to the identifiers created by `mkid`, for example
```Julia
julia> Algebra.set(Algebra.mkid(:a,3),2)
```
will give `a3` the value 2. Similarly, the `unset` statement can be used to remove the value from these identifiers, for example
```Julia
julia> Algebra.unset(Algebra.mkid(:a,3))
```

## 7.13 The Pochhammer Notation

```@docs
Reduce.Algebra.pochhammer
```

## 7.14 PF Operator

```@docs
Reduce.Algebra.pf
```

*Example:* Given `R"2/((x+1)^2*(x+2))"` in the workspace, `R"pf(ws,x)"` gives the result
```
    2      - 2         2
{-------,-------,--------------}
  x + 2   x + 1    2
                  x  + 2*x + 1
```
If you want the denominators in factored form, use `off(:exp)`. Thus, with `R"2/((x+1)^2*(x+2))"` in the workspace, the commands `R"off(exp); pf(ws,x)"` give the result
```
    2      - 2       2
{-------,-------,----------}
  x + 2   x + 1          2
                  (x + 1)
```
To recombine the terms, `for each… sum` can be used. So with the above list in the workspace, `R"for each j in ws sum j"` returns the result
```
        2
------------------
                2
 (x + 2)*(x + 1)
```
Alternatively, one can use the operations on lists to extract any desired term.

## 7.15 SELECT Operator

```@docs
Reduce.Algebra.select
```

## 7.16 SOLVE Operator

```@docs
Reduce.Algebra.solve
```

```@docs
Reduce.Algebra.root_multiplicities
```

```@docs
Reduce.multiplicities
```

### 7.16.1 Handling of Undetermined Solutions

When `solve` cannot find a solution to an equation, it normally returns an equation for the relevant indeterminates in terms of the operator `root_of`. For example, the expression
```Julia
julia> Algebra.solve(:(cos(x)+log(x)),:x)
```
returns the result
```Julia
(:(x = root_of(cos(x_) + log(x_), x_, tag_1)),)
```
An expression with a top-level `root_of` operator is implicitly a list with an unknown number of elements (since we don’t always know how many solutions an equation has). If a substitution is made into such an expression, closed form solutions can emerge. If this occurs, the `root_of` construct is replaced by an operator `one_of`. At this point it is of course possible to transform the result of the original `solve` operator expression into a standard `solve` solution. To effect this, the operator `expand_cases` can be used.

The following example shows the use of these facilities:

```Julia
julia> Algebra.solve(:(-a*x^3+a*x^2+x^4-x^3-4*x^2+4),:x)
(:(x = root_of((a * x_ ^ 2 - x_ ^ 3) + 4x_ + 4, x_, tag_2)), :(x = 1))

julia> Algebra.sub(:a=-1,ans)
(:(x=one_of((2, -1, -2), tag_2)), :(x=1))
 
julia> Algebra.expand_cases(ans)
(:(x=2), :(x=-1), :(x=-2), :(x=1))
```

### 7.16.2 Solutions of Equations Involving Cubics and Quartics

Since roots of cubics and quartics can often be very messy, a switch `fullroots` is available, that, when off (the default), will prevent the production of a result in closed form. The `root_of` construct will be used in this case instead.

In constructing the solutions of cubics and quartics, trigonometrical forms are used where appropriate. This option is under the control of a switch `trigform`, which is normally on.

The following example illustrates the use of these facilities:
```Julia
julia> Algebra.rlet(:xx => :(solve(x^3+x+1,x)))
 
julia> rcall(:xx)
(:(x = root_of(x_ ^ 3 + x_ + 1, x_, tag_1)),)

julia> Algebra.on(:fullroots)

julia> collect(rcall(:xx))
3-element Array{Expr,1}:
 :(x = -((sqrt(3) * cosh(asinh((3 * sqrt(3)) // 2) // 3) * im - sinh(asinh((3 * sqrt(3)) // 2) // 3))) // sqrt(3))
 :(x = (sqrt(3) * cosh(asinh((3 * sqrt(3)) // 2) // 3) * im + sinh(asinh((3 * sqrt(3)) // 2) // 3)) // sqrt(3))
 :(x = (-2 * sinh(asinh((3 * sqrt(3)) // 2) // 3)) // sqrt(3))

julia> Algebra.off(:trigform)
 
julia> collect(rcall(:xx))
3-element Array{Expr,1}:
 :(x = -(((sqrt(31) - 3 * sqrt(3)) ^ (2 / 3) * (sqrt(3) * im + 1) + 2 ^ (2 / 3) * (sqrt(3) * im - 1))) / (2 * (sqrt(31) - 3 * sqrt(3)) ^ (1 / 3) * 6 ^ (1 / 3) * 3 ^ (1 / 6)))
 :(x = (2 ^ (2 / 3) * (sqrt(3) * im + 1) + (sqrt(31) - 3 * sqrt(3)) ^ (2 / 3) * (sqrt(3) * im - 1)) / (2 * (sqrt(31) - 3 * sqrt(3)) ^ (1 / 3) * 6 ^ (1 / 3) * 3 ^ (1 / 6)))   
 :(x = ((sqrt(31) - 3 * sqrt(3)) ^ (2 / 3) - 2 ^ (2 / 3)) / ((sqrt(31) - 3 * sqrt(3)) ^ (1 / 3) * 6 ^ (1 / 3) * 3 ^ (1 / 6))) 
```

### 7.16.3 Other Options

If `solvesingular` is on (the default setting), degenerate systems such as `x+y=0`, `2x+2y=0` will be solved by introducing appropriate arbitrary constants. The consistent singular equation `0=0` or equations involving functions with multiple inverses may introduce unique new indeterminant kernels `arbcomplex(j)`, or `arbint(j)`, (``j=1,2,...``), representing arbitrary complex or integer numbers respectively. To automatically select the principal branches, do `off(:allbranch)`. To avoid the introduction of new indeterminant kernels do `off(:arbvars)` – then no equations are generated for the free variables and their original names are used to express the solution forms. To suppress solutions of consistent singular equations do `off(:solvesingular)`.

To incorporate additional inverse functions do, for example:
```Julia
R"put(’sinh,’inverse,’asinh)"
R"put(’asinh,’inverse,’sinh)"
```
together with any desired simplification rules such as
```
R"for all x let sinh(asinh(x))=x, asinh(sinh(x))=x"
```
For completeness, functions with non-unique inverses should be treated as `^`, `sin`, and `cos` are in the `solve` module source.

Arguments of `asin` and `acos` are not checked to ensure that the absolute value of the real part does not exceed 1; and arguments of `log` are not checked to ensure that the absolute value of the imaginary part does not exceed `π`; but checks (perhaps involving user response for non-numerical arguments) could be introduced using `let` statements for these operators.

### 7.16.4 Parameters and Variable Dependency

```@docs
Reduce.Algebra.requirements
```

```@docs
Reduce.Algebra.assumptions
```

`solve` rearranges the variable sequence to reduce the (expected) computing time. This behavior is controlled by the switch `varopt`, which is on by default. If it is turned off, the supplied variable sequence is used or the system kernel ordering is taken if the variable list is omitted. The effect is demonstrated by an example:
```Julia
julia> @rcall s=(y^3+3x=0,x^2+y^2=1);
 
julia> Algebra.solve(:s,(:y,:x)) |> collect
2-element Array{Expr,1}:
 :(y = root_of((y_ ^ 6 + 9 * y_ ^ 2) - 9, y_, tag_2))
 :(x = -(y ^ 3) // 3)    

julia> Algebra.off(:varopt); Algebra.solve(:s,(:y,:x)) |> collect
2-element Array{Expr,1}:
 :(y = (-(((x ^ 4 - 2 * x ^ 2) + 10)) * x) // 3)                     
 :(x = root_of(((x_ ^ 6 - 3 * x_ ^ 4) + 12 * x_ ^ 2) - 1, x_, tag_3))
```
In the first case, `solve` forms the solution as a set of pairs ``(y_i,x(y_i))`` because the degree of x is higher – such a rearrangement makes the internal computation of the Gröbner basis generally faster. For the second case the explicitly given variable sequence is used such that the solution has now the form `(x_i,y(x_i))`. Controlling the variable sequence is especially important if the system has one or more free variables. As an alternative to turning off `varopt`, a partial dependency among the variables can be declared using the `depend` statement: `solve` then rearranges the variable sequence but keeps any variable ahead of those on which it depends.

```Julia
julia> Algebra.on(:varopt)

julia> @rcall s=(a^3+b,b^2+c);

julia> Algebra.solve(:s,(:a,:b,:c))
(:(a = arbcomplex(1)), :(b = -(a ^ 3)), :(c = -(a ^ 6))) 

julia> Algebra.depend(:a,:c); Algebra.depend(:b,:c)

julia> Algebra.solve(:s,(:a,:b,:c))
3-element Array{Expr,1}:
 :(c = arbcomplex(2))                  
 :(a = root_of(a_ ^ 6 + c, a_, tag_3))
 :(b = -(a ^ 3)) 
```
Here `solve` is forced to put ``c`` after ``a`` and after ``b``, but there is no obstacle to interchanging ``a`` and ``b``.

## 7.17 Even and Odd Operators

```@docs
Reduce.Algebra.even
```

```@docs
Reduce.Algebra.odd
```

## 7.18 Linear Operators

```@docs
Reduce.Algebra.linear
```

To summarize, `y` “depends” on the indeterminate `x` in the above if either of the following hold:

1. `y` is an expression that contains `x` at any level as a variable, e.g.: `cos(sin(x))`
2. Any variable in the expression `y` has been declared dependent on `x` by use of the declaration `depend`.

The use of such linear operators can be seen in the paper Fox, J.A. and A. C. Hearn, “Analytic Computation of Some Integrals in Fourth Order Quantum Electrodynamics” Journ. Comp. Phys. 14 (1974) 301-317, which contains a complete listing of a program for definite integration of some expressions that arise in fourth order quantum electrodynamics.

## 7.19 Non-Commuting Operators

```@docs
Reduce.Algebra.noncom
```

The `let` statement may be used to introduce rules of evaluation for such operators. In particular, the boolean operator `ordp` is useful for introducing an ordering on such expressions.

*Example:* The rule
```Julia
R"for all x,y such that x neq y and ordp(x,y) let u(x)*u(y)= u(y)*u(x)+comm(x,y)"
```
would introduce the commutator of `u(x)` and `u(y)` for all `x` and `y`. Note that since `ordp(x,x)` is true, the equality check is necessary in the degenerate case to avoid a circular loop in the rule.

## 7.20 Symmetric and Antisymmetric Operators

```@docs
Reduce.Algebra.symmetric
```

```@docs
Reduce.Algebra.antisymmetric
```
 
## 7.21 Declaring New Prefix Operators

```@docs
Reduce.Algebra.operator
```

If the user forgets to declare an identifier as an operator, the system will prompt the user to do so in interactive mode, or do it automatically in non-interactive mode. A diagnostic message will also be printed if an identifier is declared `operator` more than once.

Operators once declared are global in scope, and so can then be referenced anywhere in the program. In other words, a declaration within a block (or a procedure) does not limit the scope of the operator to that block, nor does the operator go away on exiting the block (use `clear` instead for this purpose).

## 7.22 Declaring New Infix Operators

```@docs
Reduce.Algebra.infix
```

```@docs
Reduce.Algebra.precedence
```

Both infix and prefix operators have no transformation properties unless `let` statements or procedure declarations are used to assign a meaning.

We should note here that infix operators so defined are always binary: `R"a mm b mm c"` means `R"(a mm b) mm c"`.

## 7.23 Creating/Removing Variable Dependency

```@docs
Reduce.Algebra.depend
```

```@docs
Reduce.Algebra.nodepend
```
