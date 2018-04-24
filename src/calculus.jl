#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

const calculus = [
    :df,
    :int,
    :limit,
    :logb,
    :solve,
    :pf,
    :order,
    :korder,
    :structr,
    :coeff,
    :coeffn,
    :part,
    :realvalued,
    :notrealvalued,
    :factorize,
    :remainder,
    :resultant,
    :deg,
    :lcof,
    :lpower,
    :lterm,
    :reduct,
    :totaldeg,
    :matrix,
    :operator
]

const alg = [
    :sum,
    :prod
]

const iops = [
    :+,
    :-,
    :*,
    :^,
    :/,
    ://
]

const cmat = [
    :mateign,
    :cofactor
]

Expr(:toplevel,[:(import Base: $i) for i ∈ [alg;iops]]...) |> eval
:(export $([calculus;alg;iops;cmat]...)) |> eval
#:(export $(Symbol.("@",[calculus;alg;iops])...)) |> eval

for fun in [calculus;alg;iops]
    @eval begin
        $(parsegen(fun,:calculus))
        $(unfoldgen(fun,:calculus))
        #=macro $fun(expr,s...)
            :($$(QuoteNode(fun))($(esc(expr)),$(esc(s))...))
        end=#
    end
end

for fun in [calculus;alg]
    @eval begin
        function $fun(expr::Compat.String,s...;be=0)
            convert(Compat.String, $fun(RExpr(expr),s...;be=be))
        end
    end
end

for fun in cmat
    @eval begin
        $(parsegen(fun,:calculus))
        function $fun(expr::Union{Array{Any,2},Expr,Symbol};be=0)
            $fun(RExpr(expr),s...;be=be)
        end
    end
end

for fun in iops
    @eval begin
        function $fun(a,expr::ExprSymbol,s...;be=0)
            $fun(RExpr(a),RExpr(expr),s...;be=be) |> parse
        end
        function $fun(a::Union{<:Number,Expr,Symbol},r::RExpr,s...;be=0)
            $fun(RExpr(a),r,RExpr.(s)...;be=be)
        end
        function $fun(expr::ExprSymbol,b::ExprSymbol,s...;be=0)
            $fun(RExpr.([expr,b,s...])...;be=be) |> parse
        end
    end
end

^(expr::ExprSymbol,s::Integer;be=0) = ^(RExpr(expr),s;be=be) |> parse
^(expr::RExpr,s::Integer;be=0) = ^(expr,RExpr(s);be=be)
//(expr,b::T;be=0) where T <: AbstractFloat = //(RExpr(expr),RExpr(b);be=be) |> parse
//(a::T,expr;be=0) where T <: AbstractFloat = //(RExpr(a),RExpr(expr);be=be) |> parse
//(expr::ExprSymbol,b::T;be=0) where T <: AbstractFloat = //(RExpr(expr),RExpr(b);be=be) |> parse
//(a::T,expr::ExprSymbol;be=0) where T <: AbstractFloat = //(RExpr(a),RExpr(expr);be=be) |> parse
function //(a::T,b::T;be=0) where T <: AbstractFloat
    isnan(a) | isnan(b) | (isinf(a) & isinf(b)) && return NaN
    return //(RExpr(a),RExpr(b);be=be) |> parse |> eval
end

function solve(a::Array{T,1},s::Array{Symbol,1}) where T <: Any
    out = solve(list(a),list(s))
    return T <: String ? out.str : parse(out)
end
solve(a::T,s::Symbol) where T <: Vector = solve(a,[s])
solve(a::Expr,s::Array{Symbol,1}) = solve(a.head == :block ? a.args : [a],s)
solve(a::Expr,s::Symbol) = solve(a,[s])

order(::Void) = order(R"nil") |> parse
korder(::Void) = korder(R"nil") |> parse

export ∑, ∏

(∑, ∏) = (sum, prod)
