#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

VERSION >= v"0.7.0-DEV.4445" && (import LinearAlgebra: factorize)

const sbas = [
    :abs,
    :conj,
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
    :atan2,
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
    :gamma,
]

const sdep = [
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
]

const snan = [
    :rlet,
    :clearrules,
    :scientific_notation,
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

Expr(:block,[:($i(r)=Base.$i(r)) for i ∈ [sbas;sdep;sbat;[:length]]]...) |> eval
#Expr(:toplevel,[:(import Base: $i) for i ∈ [sbas;sdep;sbat;[:length]]]...) |> eval
:(export $([sbas;sdep;sfun;snan;snum;scom;sint;sran;sbat;smat;[:length]]...)) |> eval
#:(export $(Symbol.("@",[sbas;sdep;sfun;snum;scom;sint])...)) |> eval

for fun in [sbas;sdep;sfun;snum;scom;sint;sran;sbat;smat;[:length]]
    Reduce.parsegen(fun,:unary) |> eval
end

for fun in [sbas;sdep;sfun;snum;scom;sint]
    @eval begin
        $(Reduce.unfoldgen(fun,:unary))
        function $fun(expr::Compat.String;be=0)
            convert(Compat.String, $fun(RExpr(expr);be=be))
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

tp(r::Array{T,1}) where T <: ExprSymbol = r |> RExpr |> tp |> parse |> mat
tp(r::Union{Vector,RowVector}) = r |> RExpr |> tp |> parse |> mat

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

Unlike substitutions via `sub`, `rlet` rules are global in scope and stay in effect until replaced or `clear`ed.
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
scientific_notation(r::Union{Vector,RowVector}) = scientific_notation(list(r)) |> parse
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
            "$x" |> parse |> RExpr |> $fun |> parse |> eval
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
