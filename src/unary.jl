#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

#import LinearAlgebra: besseli, besselj, bessely, polygamma, zeta, factorize

sbas = [
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
    :factorize
]

sdep = [
    :beta,
    :besseli,
    :besselj,
    :besselk,
    :bessely,
    :polygamma,
    :zeta
]

sfun = [
#    :logb,
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
    :sphericalharmonicy
]

snum = [
    :ceiling,
    :fix,
]

scom = [
    :impart,
    :repart,
]

sint = [
    :nextprime,
    :euler,
    :fibonacci,
    :motzkin,
]

sran = [
    :random,
    :random_new_seed
]

Expr(:toplevel,[:(import Base: $i) for i ∈ [sbas;sdep;[:length]]]...) |> eval
:(export $([sbas;sdep;sfun;snum;scom;sint;sran;[:length]]...)) |> eval

for fun in [sbas;sdep;sfun;snum;scom;sint;sran;[:length]]
    parsegen(fun,:unary) |> eval
end

for fun in [sbas;sdep;sfun;snum;scom;sint]
    quote
        function $fun(expr::Compat.String;be=0)
            convert(Compat.String, $fun(RExpr(expr);be=be))
        end
    end |> eval
    unfoldgen(fun,:unary) |> eval
end

length(r::Expr;be=0) = length(r |> RExpr;be=be) |> parse |> eval

for fun in [sint;sran]
    quote
        function $fun(n::T) where T <: Integer
            convert(T, $fun(RExpr(n)) |> parse |> eval)
        end
    end |> eval
end

for fun in snum
    quote
        function $fun(x::T) where T <: Real
            $fun(RExpr(x)) |> parse |> eval
        end
    end |> eval
end

for fun in scom
    quote
        function $fun(x::T) where T <: Number
            "$x" |> parse |> RExpr |> $fun |> parse |> eval
        end
    end |> eval
end

function bernoulli(n::T) where T <: Integer
    bernoulli(RExpr(n)) |> parse |> eval
end
