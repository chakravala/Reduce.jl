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
    :depend,
    :arglength,
    :decompose,
    :num,
    :den,
    :mainvar,
    :precision,
    :setmod,
    :rootval,
    :clear
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
:(export $([sbas;sdep;sfun;snum;scom;sint;sran;sbat;smat;[:length]]...)) |> eval
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

import Base.LinAlg: transpose, ctranspose

tp(r::Array{T,1}) where T <: ExprSymbol = r |> RExpr |> tp |> parse |> mat
tp(r::Union{Vector,RowVector}) = r |> RExpr |> tp |> parse |> mat
transpose(r::ExprSymbol) = r
ctranspose(r::ExprSymbol) = conj(r)

length(r::Expr) = length(r |> RExpr) |> parse |> eval

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
