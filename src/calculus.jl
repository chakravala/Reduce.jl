#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

calculus = Symbol[
    :df,
    :int
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
        function $fun(a,expr::Union{<:Expr,<:Symbol},s...;be=0)
            $fun(RExpr(a),RExpr(expr),s...;be=be) |> parse
        end
        function $fun(expr::Union{<:Expr,<:Symbol},b::Union{<:Expr,<:Symbol},s...;be=0)
            $fun(RExpr.([expr,b,s...])...;be=be) |> parse
        end
    end
end

^(expr::Union{<:Expr,<:Symbol},s::Integer;be=0) = ^(RExpr(expr),s;be=be) |> parse

export ∑, ∏

(∑, ∏) = (sum, prod)
