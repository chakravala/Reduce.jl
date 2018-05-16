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
