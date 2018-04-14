#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

calculus = Symbol[
    :df,
    :int,
    :limit
]

alg = Symbol[
    :sum,
    :prod
]

iops = Symbol[
    :+,
    :-,
    :*,
    :^,
    :/,
    ://
]

Expr(:toplevel,[:(import Base: $i) for i ∈ [alg;iops]]...) |> eval
:(export $([calculus;alg;iops]...)) |> eval
:(export $(Symbol.("@",[calculus;alg;iops])...)) |> eval

for fun in [calculus;alg;iops]
    parsegen(fun,:calculus) |> eval
    unfoldgen(fun,:calculus) |> eval
    @eval begin
        macro $fun(expr,s...)
            :($$(QuoteNode(fun))($(esc(expr)),$(esc(s))...))
        end
    end
end

for fun in [calculus;alg]
    @eval begin
        function $fun(expr::Compat.String,s...;be=0)
            convert(Compat.String, $fun(RExpr(expr),s...;be=be))
        end
    end
end

for fun in iops
    @eval begin
        function $fun(a,expr::ExprSymbol,s...;be=0)
            $fun(RExpr(a),RExpr(expr),s...;be=be) |> parse
        end
        function $fun(expr::ExprSymbol,b::ExprSymbol,s...;be=0)
            $fun(RExpr.([expr,b,s...])...;be=be) |> parse
        end
    end
end

^(expr::ExprSymbol,s::Integer;be=0) = ^(RExpr(expr),s;be=be) |> parse
//(expr,b::T;be=0) where T <: AbstractFloat = //(RExpr(expr),RExpr(b);be=be) |> parse
//(a::T,expr;be=0) where T <: AbstractFloat = //(RExpr(a),RExpr(expr);be=be) |> parse
//(expr::ExprSymbol,b::T;be=0) where T <: AbstractFloat = //(RExpr(expr),RExpr(b);be=be) |> parse
//(a::T,expr::ExprSymbol;be=0) where T <: AbstractFloat = //(RExpr(a),RExpr(expr);be=be) |> parse
function //(a::T,b::T;be=0) where T <: AbstractFloat
    isnan(a) | isnan(b) | (isinf(a) & isinf(b)) && return NaN
    out = //(RExpr(a),RExpr(b);be=be) |> parse |> eval
    if out == nothing
        c = 1
        f = SubFail()
        h = SubHold()
        while out == nothing && c < f
            sleep(sqrt(c)*h)
            out = //(RExpr(a),RExpr(b);be=be) |> parse |> eval
            c += 1
        end
        PipeClogged(out ≠ nothing, c, "rational division")
        out == nothing && throw(ReduceError("If generated code relies on many floating point divisions, try setting `Reduce.Rational(false)` and use `Reduce.Reset()`, since rational division is the default."))
    end
    return out
end

export ∑, ∏

(∑, ∏) = (sum, prod)
