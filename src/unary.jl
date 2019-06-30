#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

import LinearAlgebra: factorize

const sbas = [
    :conj,
    :abs,
    :factorial,
    :floor,
    :max,
    :min,
    :round,
    :sign,
    :acos,
    :acosh,
    :acot,
    :acoth,
    :acsc,
    :acsch,
    :asec,
    :asech,
    :asin,
    :asinh,
    :atan,
    :atanh,
    #:atan2,
    :cos,
    :cosh,
    :cot,
    :coth,
    :csc,
    :csch,
    :exp,
    :hypot,
    :log,
    :log10,
    :sec,
    :sech,
    :sin,
    :sinh,
    :sqrt,
    :tan,
    :tanh,
]

const sdep = [
    :gamma,
    :beta,
    :besseli,
    :besselj,
    :besselk,
    :bessely,
    :polygamma,
    :zeta
]

const sfun = [
    :ibeta,
    :igamma,
    :ln,
    :psi,
    :bernoulli,
    :continued_fraction,
    :ci, #
    :dilog,
    :ei,
    :si,
    :airy_ai,
    :airy_aiprime,
    :airy_bi,
    :airy_biprime,
    :hanekl1,
    :hankel2,
    :kummerm,
    :kummeru,
    :lommel1,
    :lommel2,
    :struveh,
    :struvel,
    :whittakerm,
    :whittakeru,
    :solidharmonicy,
    :sphericalharmonicy,
    :expand_cases,
    :arglength,
    :decompose,
    :num,
    :den,
    :mainvar,
    :precision,
    :setmod,
    :rootval,
    :showrules,
    :reverse,
    :lhs,
    :rhs,
    :saveas,
    :root_val,
]

const snan = [
    :rlet,
    :clearrules,
    :scientific_notation,
    :optimize,
]

const snum = [
    :ceiling,
    :fix,
]

const scom = [
    :impart,
    :repart,
]

const sint = [
    :nextprime,
    :euler,
    :fibonacci,
    :motzkin,
]

const sran = [
    :random,
    :random_new_seed
]

const sbat = [
    :det,
    :trace,
    :nullspace,
    :rank
]

const smat = [
    :tp,
]

Expr(:block,[:($i(r)=Base.$i(r)) for i ∈ [sbas[2:end];sdep;sbat;[:length]]]...) |> eval
#Expr(:toplevel,[:(import Base: $i) for i ∈ [sbas;sdep;sbat;[:length]]]...) |> eval
:(export $([sbas;sdep;sfun;snan;snum;scom;sint;sran;sbat;smat;[:length]]...)) |> eval
#:(export $(Symbol.("@",[sbas;sdep;sfun;snum;scom;sint])...)) |> eval

for fun in [sbas;sdep;sfun;snum;scom;sint;sran;sbat;smat;[:length]]
    Reduce.parsegen(fun,:unary) |> eval
end

for fun in [sbas;sdep;sfun;snum;scom;sint]
    @eval begin
        $(Reduce.unfoldgen(fun,:unary))
        function $fun(expr::String;be=0)
            convert(String, $fun(RExpr(expr);be=be))
        end
        #=macro $fun(expr)
            :($$(QuoteNode(fun))($(esc(expr))))
        end=#
    end
end

for fun in [sbat;smat]
    @eval begin
        function $fun(expr::Union{Array{T,2},T}) where T <: ExprSymbol
            $fun(RExpr(expr)) |> parse |> mat
        end
        function $fun(expr::Array{Any,2})
            $fun(RExpr(expr)) |> parse |> mat
        end
    end
end

"""
    tp(exprn)

Syntax:
```
        tp(EXPRN:matrix_expression):matrix.
```
This operator takes a single matrix argument and returns its transpose.
"""
tp(r::Array{T,1}) where T <: ExprSymbol = r |> RExpr |> tp |> parse |> mat
tp(r::Union{Vector,Adjoint}) = r |> RExpr |> tp |> parse |> mat

"""
    length(r)

`length` is a generic operator for finding the length of various objects in the system. The meaning depends on the type of the object. In particular, the length of an algebraic expression is the number of additive top-level terms its expanded representation.

*Examples:*
```Julia
julia> length(:(a+b))
2

julia> length(2)
1
```
Other objects that support a length operator include arrays, lists and matrices. The explicit meaning in these cases is included in the description of these objects.
"""
length(r::Expr) = length(r |> RExpr) |> parse |> eval

for fun in snan
    nfun = fun ≠ :rlet ? fun : :let
    @eval begin
        $fun(r::RExpr) = string($(string(nfun)),"(",string(r),")") |> rcall |> RExpr
        $fun(r) = $fun(RExpr(r)) |> parse
    end
end

"""
    rlet(::Union{Dict,Pair},expr)

The simplest use of the `let` statement is in the form
```
R"let ⟨substitution list⟩"
```
where `⟨substitution list⟩` is a list of rules separated by commas, each of the form:
```
⟨variable⟩ = ⟨expression⟩
```
or
```
⟨prefix operator⟩(⟨argument⟩,…,⟨argument⟩) = ⟨expression⟩
```
or
```
⟨argument⟩⟨infix operator⟩,…,⟨argument⟩ = ⟨expression⟩
```
For example,
```
        let {x => y^2,
             h(u,v) => u - v,
             cos(pi/3) => 1/2,
             a*b => c,
             l+m => n,
             w^3 => 2*z - 3,
             z^10 => 0}
```
The list brackets can be left out if preferred. The above rules could also have been entered as seven separate `let` statements.

After such `let` rules have been input, `x` will always be evaluated as the square of `y`, and so on. This is so even if at the time the `let` rule was input, the variable `y` had a value other than `y`. (In contrast, the assignment `R"x:=y^2"` will set `x` equal to the square of the current value of `y`, which could be quite different.)

The rule `let a*b=c` means that whenever `a` and `b` are both factors in an expression their product will be replaced by `c`. For example, `a^5*b^7*w` would be replaced by `c^5*b^2*w`.

The rule for `l+m` will not only replace all occurrences of `l+m` by `n`, but will also normally replace `l` by `n-m`, but not `m` by `n-l`. A more complete description of this case is given in Section 11.2.5.

The rule pertaining to `w^3` will apply to any power of `w` greater than or equal to the third.

Note especially the last example, `let z^10=0`. This declaration means, in effect: ignore the tenth or any higher power of `z`. Such declarations, when appropriate, often speed up a computation to a considerable degree. (See Section 11.4 for more details.)

Any new operators occurring in such `let` rules will be automatically declared `operator` by the system, if the rules are being read from a file. If they are being entered interactively, the system will ask `Declare… Operator?`. Answer `Y` or `N` and hit `<Return>`.

In each of these examples, substitutions are only made for the explicit expressions given; i.e., none of the variables may be considered arbitrary in any sense. For example, the command
```Julia
julia> Algebra.rlet( :(h(u,v)) => :(u - v) )
```
will cause `h(u,v)` to evaluate to `u - v`, but will not affect `h(u,z)` or `h` with any arguments other than precisely the symbols `u,v`.

These simple `let` rules are on the same logical level as assignments made with the `:=` operator. An assignment `R"x := p+q"` cancels a rule `rlet( :x => :(y^2) )` made earlier, and vice versa.
"""
rlet(r::Dict{String,String}) = rlet(sub_list(r))
rlet(s::Dict{<:Any,<:Any}) = rlet(Dict([=>(string.(RExpr.([b[1],b[2]]))...) for b ∈ collect(s)]...))
rlet(s::Pair{<:Any,<:Any}) = rlet(Tuple([s]))
rlet(s::T) where T <: Tuple = rlet(RExpr(s)) |> parse
rlet(s::Array{<:Pair{<:Any,<:Any},1}) = rlet(Tuple(s))

"""
    Algebra.scientific_notation(::Union{Number,Tuple,Vector})

The declaration `scientific_notation` controls the output format of floating point numbers. At the default settings, any number with five or less digits before the decimal point is printed in a fixed-point notation, e.g., `12345.6`. Numbers with more than five digits are printed in scientific notation, e.g., `1.234567E+5`. Similarly, by default, any number with eleven or more zeros after the decimal point is printed in scientific notation. To change these defaults, `scientific_notation` can be used in one of two ways.
```Julia
julia> Algebra.scientific_notation(m);
```
where `m` is a positive integer, sets the printing format so that a number with more than `m` digits before the decimal point, or `m` or more zeros after the decimal point, is printed in scientific notation.
```Julia
julia> Algebra.scientific_notation(m,n);
```
with `m` and `n` both positive integers, sets the format so that a number with more than `m` digits before the decimal point, or `n` or more zeros after the decimal point is printed in scientific notation.
"""
scientific_notation(r::T) where T <: Tuple = scientific_notation(RExpr(r)) |> parse
scientific_notation(r::Union{Vector,Adjoint}) = scientific_notation(list(r)) |> parse
scientific_notation(r::T...) where T <: Number = scientific_notation(list(r)) |> parse

for fun in [sint;sran]
    @eval begin
        function $fun(n::T) where T <: Integer
            convert(T, $fun(RExpr(n)) |> parse |> eval)
        end
    end
end

for fun in snum
    @eval begin
        function $fun(x::T) where T <: Real
            $fun(RExpr(x)) |> parse |> eval
        end
    end
end

for fun in scom
    @eval begin
        function $fun(x::T) where T <: Number
            "$x" |> Meta.parse |> RExpr |> $fun |> parse |> eval
        end
    end
end

function bernoulli(n::T) where T <: Integer
    bernoulli(RExpr(n)) |> parse |> eval
end

@doc """
    lhs(::Union{Expr,RExpr})

Returns the left-hand side of an equation.

## Examples
```Julia
julia> Algebra.lhs(R"a+b=c")

a + b

```
""" Reduce.Algebra.lhs

@doc """
    rhs(::Union{Expr,RExpr})

Returns the right-hand side of an equation.

## Examples
```Julia
julia> Algebra.rhs(R"a+b=c")

c

```
""" Reduce.Algebra.rhs

@doc """
    abs(r)

`abs` returns the absolute value of its single argument, if that argument has a numerical value. A non-numerical argument is returned as an absolute value, with an overall numerical coefficient taken outside the absolute value operator. For example:

```Julia
julia> Algebra.abs(-3/4)
0.75

julia> Algebra.abs(:(2a))
:(2 * abs(a))

julia> Algebra.abs(im)
1.0

julia> Algebra.abs(:(-x))
:(abs(x))
```
""" Reduce.Algebra.abs

@doc """
    ceiling(r)

This operator returns the ceiling (i.e., the least integer greater than the given argument) if its single argument has a numerical value. A non-numerical argument is returned as an expression in the original operator. For example:

```Julia
julia> Algebra.ceiling(-5/4)
-1

julia> Algebra.ceiling(:(-a))
:(ceiling(-a))
```
""" Reduce.Algebra.ceiling

@doc """
    conj(r)

This operator returns the ceiling (i.e., the least integer greater than the given argument) if its single argument has a numerical value. A non-numerical argument is returned as an expression in the original operator. For example:

```Julia
julia> Algebra.conj(1+im)
1 - 1im

julia> Algebra.conj(:(a+im*b))
:(repart(a) - ((impart(a) + repart(b)) * im + impart(b)))
```
""" Reduce.Algebra.conj

@doc """
    factorial(r)

If the single argument of `factorial` evaluates to a non-negative integer, its factorial is returned. Otherwise an expression involving `factorial` is returned. For example:

```Julia
julia> Algebra.factorial(5)
120

julia> Algebra.factorial(:a)
:(factorial(a))
```
""" Reduce.Algebra.factorial

@doc """
    fix(r)

This operator returns the fixed value (i.e., the integer part of the given argument) if its single argument has a numerical value. A non-numerical argument is returned as an expression in the original operator. For example:

```Julia
julia> Algebra.fix(-5/4)
-1

julia> Algebra.fix(:a)
:(fix(a))
```
""" Reduce.Algebra.fix

@doc """
    floor(r)

This operator returns the floor (i.e., the greatest integer less than the given argument) if its single argument has a numerical value. A non-numerical argument is returned as an expression in the original operator. For example:

```Julia
julia> Algebra.floor(-5/4)
-2.0

julia> Algebra.floor(:a)
:(floor(a))
```
""" Reduce.Algebra.floor

@doc """
    impart(r)

This operator returns the imaginary part of an expression, if that argument has an numerical value. A non-numerical argument is returned as an expression in the operators `repart` and `impart`. For example:

```Julia
julia> Algebra.impart(1+im)
1

julia> Algebra.impart(:(a+im*b))
:(impart(a) + repart(b))
```
""" Reduce.Algebra.impart

@doc """
    nextprime(r)

`nextprime` returns the next prime greater than its integer argument, using a probabilistic algorithm. A type error occurs if the value of the argument is not an integer. For example:

```Julia
julia> Algebra.nextprime(5)
7

julia> Algebra.nextprime(-2)
2

julia> Algebra.nextprime(-7)
-5

julia> Algebra.nextprime(1000000)
1000003
```
whereas `Algebra.nextprime(:a)` gives a type error.
""" Reduce.Algebra.nextprime

@doc """
    random(r)


`random(n)` returns a random number ``r`` in the range ``0 ≤ r < n``. A type error occurs if the value of the argument is not a positive integer in algebraic mode, or positive number in symbolic mode. For example:

```Julia
julia> Algebra.random(5)
3

julia> Algebra.random(1000)
191
```
whereas `Algebra.random(:a)` gives a type error.
""" Reduce.Algebra.random

@doc """
    random_new_seed(r)

`random_new_seed(n)` reseeds the random number generator to a sequence determined by the integer argument `n`. It can be used to ensure that a repeatable pseudo-random sequence will be delivered regardless of any previous use of `random`, or can be called early in a run with an argument derived from something variable (such as the time of day) to arrange that different runs of a REDUCE program will use different random sequences. When a fresh copy of REDUCE is first created it is as if `random_new_seed(1)` has been obeyed.

A type error occurs if the value of the argument is not a positive integer.
""" Reduce.Algebra.random_new_seed

@doc """
    repart(r)

This returns the real part of an expression, if that argument has an numerical value. A non-numerical argument is returned as an expression in the operators `repart` and `impart`. For example:

```Julia
julia> Algebra.repart(1+im)
1

julia> Algebra.repart(:(a+im*b))
:(repart(a) - impart(b))
```
""" Reduce.Algebra.repart

@doc """
    round(r)

This operator returns the rounded value (i.e, the nearest integer) of its single argument if that argument has a numerical value. A non-numeric argument is returned as an expression in the original operator. For example:

```Julia
julia> Algebra.round(-5/4)
-1.0

julia> Algebra.round(:a)
:(round(a))
```
""" Reduce.Algebra.round

@doc """
    sign(r)

`sign` tries to evaluate the sign of its argument. If this is possible `sign` returns one of `1`, `0` or `-1`. Otherwise, the result is the original form or a simplified variant. For example:

```Julia
julia> Algebra.sign(-5)
-1

julia> Algebra.sign(:(-a^2*b))
:(-(sign(b)))
```
Note that even powers of formal expressions are assumed to be positive only as long as the switch `complex` is off.
""" Reduce.Algebra.sign

@doc """
    bernoulli(n)

The unary operator `bernoulli` provides notation and computation for Bernoulli numbers. `bernoulli(n)` evaluates to the `n`th Bernoulli number; all of the odd Bernoulli numbers, except `bernoulli(1)`, are zero.

The algorithms are based upon those by Herbert Wilf, presented by Sandra Fillebrown [?]. If the `rounded` switch is off, the algorithms are exactly those; if it is on, some further rounding may be done to prevent computation of redundant digits. Hence, these functions are particularly fast when used to approximate the Bernoulli numbers in rounded mode.
""" Reduce.Algebra.bernoulli

@doc """
    fibonacci(n)

The unary operator `fibonacci` provides notation and computation for Fibonacci numbers. `fibonacci(n)` evaluates to the `n`th Fibonacci number. If `n` is a positive or negative integer, it will be evaluated following the definition:

\$F_0 = 0; F_1 = 1; F_n = F_{n-1} + F_{n-2}\$
""" Reduce.Algebra.fibonacci

@doc """
    motzkin(n)

A Motzkin number \$M_n\$ (named after Theodore Motzkin) is the number of different ways of drawing non-intersecting chords on a circle between n points. For a non-negative integer `n`, the operator `motzkin(n)` returns the `n`th Motzkin number, according to the recursion formula

\$M_0 =  1;  M_1 = 1;  M_{n+1}  =  \\frac{2n+3}{n+3}M_n + \\frac{3n}{n+3}M_{n-1}.\$
""" Reduce.Algebra.motzkin

@doc """
    saveas(expr)

If the user wishes to assign the workspace to a variable or expression for later use, the `saveas` statement can be used. It has the syntax
```Julia
R"saveas ⟨expression⟩"
```
For example, after the differentiation in the last example, the workspace holds the expression `2*x+2*y`. If we wish to assign this to the variable `z` we can now say
```Julia
julia> Algebra.saveas(:z)
```
If the user wishes to save the expression in a form that allows him to use some of its variables as arbitrary parameters, the `for all` command can be used.

*Example:*
```Julia
R"for all x saveas h(x)"
```
with the above expression would mean that `h(z)` evaluates to `2*y+2*z`.
""" Reduce.Algebra.saveas

@doc """
    decompose(p)

The `decompose` operator takes a multivariate polynomial as argument, and returns an expression and a list of equations from which the original polynomial can be found by composition. Its syntax is:
```Julia
R"decompose(EXPRN:polynomial)"
```
For example:
```Julia
julia> Algebra.decompose(:(x^8-88*x^7+2924*x^6-43912*x^5+263431*x^4-218900*x^3+65690*x^2-7700*x+234))
(:(u ^ 2 + 35u + 234), :(u = v ^ 2 + 10v), :(v = x ^ 2 - 22x))

julia> Algebra.decompose(:(u^2+v^2+2u*v+1))
(:(w ^ 2 + 1), :(w = u + v))
```
Users should note however that, unlike factorization, this decomposition is not unique.
""" Reduce.Algebra.decompose

den(r::T) where T <: Number = den(RExpr(r)) |> parse

@doc """
    den(r)

This is used with the syntax:
```Julia
R"den(EXPRN:rational)"
```
It returns the denominator of the rational expression `EXPRN`. If `EXPRN` is a polynomial, 1 is returned.

*Examples:*
```Julia
julia> Algebra.den(:(x/y^2))
:(y ^ 2)

julia> Algebra.den(100//6)
3               [since 100/6 is first simplified to 50/3]  

julia> Algebra.den(:(a/4+b/6))
12

julia> Algebra.den(:(a+b))
1
```
""" Reduce.Algebra.den

mainvar(r::T) where T <: Number = mainvar(RExpr(r)) |> parse

@doc """
    mainvar(exprn)

Syntax:
```Julia
R"mainvar(EXPRN:polynomial)"
```
Returns the main variable (based on the internal polynomial representation) of `EXPRN`. If `EXPRN` is a domain element, 0 is returned.

*Examples:* Assuming `a` has higher kernel order than `b`, `c`, or `d`:
```Julia
julia> Algebra.on(:exp)

julia> Algebra.mainvar(:((a+b)*(c+2*d)^2))
:a

julia> Algebra.mainvar(2)
0
```
""" Reduce.Algebra.mainvar

num(r::T) where T <: Number = num(RExpr(r)) |> parse

@doc """
    num(exprn)

Syntax:
```Julia
R"num(EXPRN:rational)"
```
Returns the numerator of the rational expression `EXPRN`. If `EXPRN` is a polynomial, that polynomial is returned.

*Examples:*
```Julia
julia> Algebra.num(:(x/y^2))
:x

julia> Algebra.num(100//6)
50

julia> Algebra.num(:(a/4+b/6))
:(3a + 2b)

julia> Algebra.num(:(a+b))
:(a + b)
```
""" Reduce.Algebra.num

setmod(r::Integer) = setmod(RExpr(r)) |> parse

@doc """
    setmod(::Integer)

REDUCE includes facilities for manipulating polynomials whose coefficients are computed modulo a given base. To use this option, two commands must be used; `R"setmod ⟨integer⟩"`, to set the prime modulus, and `on(:modular)` to cause the actual modular calculations to occur. For example, with `R"setmod 3"` and `R"on modular"`, the polynomial `(a+2*b)^3` would become `a^3+2*n^3`.

The argument of `setmod` is evaluated algebraically, except that non-modular (integer) arithmetic is used. Thus the sequence
```Julia
R"setmod 3; on modular; setmod 7"
```
will correctly set the modulus to 7.
""" Reduce.Algebra.setmod

@doc """
    root_val(exprn)

The `root_val` operator takes a single univariate polynomial as argument, and returns a list of root values at system precision (or greater if required to separate roots). It is used with the syntax
```Julia
R"root_val(EXPRN:univariate polynomial)"
```
For example, the sequence
```Julia
reduce> on rounded; root_val(x^3-x-1);
```
gives the result
```
{0.562279512062*I - 0.662358978622, - 0.562279512062*I  
 
  - 0.662358978622,1.32471795724}
```
""" Reduce.Algebra.root_val

@doc """
    clearrules(r)

`clearrules` has the syntax
```Julia
R"clearrules <rule list>|<name of rule list>(,...)"
```
""" Reduce.Algebra.clearrules

@doc """
    showrules(r)

The operator `showrules` takes a single identifier as argument, and returns in rule-list form the operator rules associated with that argument. For example:
```Julia
reduce> showrules log;  
 
{log(e) => 1,  
 
 log(1) => 0,  
 
      ~x  
 log(e  ) => ~x,  
 
                    1  
 df(log(~x),~x) => ----}  
                    ~x
```
Such rules can then be manipulated further as with any list. For example `R"rhs first ws"` has the value `1`. Note that an operator may have other properties that cannot be displayed in such a form, such as the fact it is an odd function, or has a definition defined as a procedure.
""" Reduce.Algebra.showrules

@doc """
    det(exprn)

Syntax:
```
        det(EXPRN:matrix_expression):algebraic.
```
The operator `det` is used to represent the determinant of a square matrix expression. E.g.,
```
Algebra.det(:(y^2))
```
is a scalar expression whose value is the determinant of the square of the matrix `y`, and
```
Algebra.det([:a :b :c; :d :e :f; :g :h :j])
```
is a scalar expression whose value is the determinant of the matrix
```Julia
3×3 Array{Symbol,2}:
 :a  :b  :c
 :d  :e  :f
 :g  :h  :j
```
Determinant expressions have the instant evaluation property. In other words, the statement
```
        let det mat((a,b),(c,d)) = 2;
```
sets the value of the determinant to `2`, and does not set up a rule for the determinant itself.
""" Reduce.Algebra.det

@doc """
    trace(exprn)

Syntax:
```
        TRACE(EXPRN:matrix_expression):algebraic.
```
The operator TRACE is used to represent the trace of a square matrix.
""" Reduce.Algebra.trace

@doc """
    nullspace(exprn)

Syntax:
```
        NULLSPACE(EXPRN:matrix_expression):list
```
`nullspace` calculates for a matrix `a` a list of linear independent vectors (a basis) whose linear combinations satisfy the equation \$Ax = 0\$. The basis is provided in a form such that as many upper components as possible are isolated.

Note that with `b := nullspace a` the expression `length b` is the *nullity* of `a`, and that `second length a - length b` calculates the *rank* of `a`. The rank of a matrix expression can also be found more directly by the `rank` operator described below.

*Example:* The command
```
        nullspace mat((1,2,3,4),(5,6,7,8));
```
gives the output
```
        {  
         [ 1  ]  
         [ 0  ]  
         [ - 3]  
         [ 2  ]  
         ,  
         [ 0  ]  
         [ 1  ]  
         [ - 2]  
         [ 1  ]  
         }
```
In addition to the REDUCE matrix form, `nullspace` accepts as input a matrix given as a list of lists, that is interpreted as a row matrix. If that form of input is chosen, the vectors in the result will be represented by lists as well. This additional input syntax facilitates the use of `nullspace` in applications different from classical linear algebra.
""" Reduce.Algebra.nullspace

@doc """
    rank(exprn)

Syntax:
```
        RANK(EXPRN:matrix_expression):integer
```
`rank` calculates the rank of its argument, that, like `nullspace` can either be a standard matrix expression, or a list of lists, that can be interpreted either as a row matrix or a set of equations.

*Example:*
```Julia
Algebra.rank([:a :b :c; :d :e :f])
```
returns the value `2`.
""" Reduce.Algebra.rank
