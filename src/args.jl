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
    :mateigen,
    :cofactor
]

Expr(:block,[:($i(r...)=Base.$i(r...)) for i ∈ [alg;iops]]...) |> eval
#Expr(:toplevel,[:(import Base: $i) for i ∈ [alg;iops]]...) |> eval
:(export $([calculus;alg;iops;cmat]...)) |> eval
#:(export $(Symbol.("@",[calculus;alg;iops])...)) |> eval

for fun in [calculus;alg;iops]
    @eval begin
        $(Reduce.parsegen(fun,:args))
        $(Reduce.unfoldgen(fun,:args))
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
        $(Reduce.parsegen(fun,:args))
        function $fun(expr::Union{Array{Any,2},Expr,Symbol})
            $fun(RExpr(expr),s...)
        end
    end
end

for fun in iops
    @eval begin
        function $fun(a::Union{<:Number,Expr,Symbol},r::RExpr,s...)
            $fun(RExpr(a),r,RExpr.(s)...) |> parse
        end
        function $fun(a::T,b::ExprSymbol,s...) where T <: Number
            $fun(RExpr(a),RExpr(b),RExpr.(s)...) |> parse
        end
    end
end

const MatExpr = Union{Array{Any,2},Array{Expr,2},Array{Symbol,2},Expr,Symbol,<:Number}
const Mat = Union{Vector,RowVector,Array{Any,2},Array{Expr,2},Array{Symbol,2}}
const MatOnly = Union{Array{Any,2},Array{Expr,2},Array{Symbol,2}}
const ESN = Union{Expr,Symbol,<:Number}

^(expr::Array{Any,2},s::Integer) = ^(RExpr(expr),s) |> parse |> mat
^(expr::Array{T,2},s::Integer) where T <: ExprSymbol = ^(RExpr(expr),s) |> parse |> mat
^(expr::RExpr,s::Integer) = ^(expr,RExpr(s))
^(a::T,s::ExprSymbol) where T <: Number = ^(RExpr(a),RExpr(s)) |> parse

*(a::T,s::S) where T <: MatOnly where S <: ESN = *(RExpr(a),RExpr(s)) |> parse |> mat
*(a::T,s::S) where T <: ESN where S <: MatOnly = *(RExpr(a),RExpr(s)) |> parse |> mat
*(a::T,s::S) where T <: MatOnly where S <: MatOnly = *(RExpr(a),RExpr(s)) |> parse |> mat
*(a::T,s::S) where T <: Vector where S <: RowVector = *(RExpr(a),RExpr(s)) |> parse |> mat
*(a::T,s::S) where T <: Vector where S <: MatExpr = *(RExpr(a),RExpr(s)) |> parse |> mat
*(a::T,s::S) where T <: RowVector where S <: Vector = *(RExpr(a),RExpr(s)) |> parse |> mat
*(a::T,s::S) where T <: RowVector where S <: MatExpr = *(RExpr(a),RExpr(s)) |> parse |> mat
*(a::T,s::S) where T <: MatExpr where S <: Vector = *(RExpr(a),RExpr(s)) |> parse |> mat
*(a::T,s::S) where T <: MatExpr where S <: RowVector = *(RExpr(a),RExpr(s)) |> parse |> mat

for o in [:+,:-]
    @eval begin
        function $o(a::T,s::S) where T <: MatOnly where S <: ESN
            $o(RExpr(a),RExpr(s)*RExpr(ones(size(a)))) |> parse |> mat
        end
        function $o(a::T,s::S) where T <: ESN where S <: MatOnly
            $o(RExpr(a)*RExpr(ones(size(s))),RExpr(s)) |> parse |> mat
        end
        $o(a::T,s::S) where T <: MatOnly where S <: MatOnly = $o(RExpr(a),RExpr(s)) |> parse |> mat
        $o(a::T,s::S) where T <: Vector where S <: RowVector = $o(RExpr(a),RExpr(s)) |> parse |> mat
        $o(a::T,s::S) where T <: Vector where S <: MatOnly = $o(RExpr(a),RExpr(s)) |> parse |> mat
        function $o(a::T,s::ESN) where T <: Vector
            $o(RExpr(a),RExpr(s)*RExpr(ones(size(a)))) |> parse |> mat
        end
        $o(a::T,s::S) where T <: RowVector where S <: Vector = $o(RExpr(a),RExpr(s)) |> parse |> mat
        $o(a::T,s::S) where T <: RowVector where S <: MatOnly = $o(RExpr(a),RExpr(s)) |> parse |> mat
        function $o(a::T,s::ESN) where T <: RowVector
            $o(RExpr(a),RExpr(s)*RExpr(ones(size(a)))) |> parse |> mat
        end
        $o(a::T,s::S) where T <: MatOnly where S <: Vector = $o(RExpr(a),RExpr(s)) |> parse |> mat
        function $o(a::ESN,s::S) where S <: Vector
            $o(RExpr(a)*RExpr(ones(size(s))),RExpr(s)) |> parse |> mat
        end
        $o(a::T,s::S) where T <: MatOnly where S <: RowVector = $o(RExpr(a),RExpr(s)) |> parse |> mat
        function $o(a::ESN,s::S) where S <: RowVector
            $o(RExpr(a)*RExpr(ones(size(s))),RExpr(s)) |> parse |> mat
        end
    end
end

for o in [:/,://]
    @eval begin
        $o(a::MatOnly,b::ESN) = $o(RExpr(a),RExpr(b)) |> parse |> mat
    end
end
//(expr,b::T) where T <: AbstractFloat = //(RExpr(expr),RExpr(b)) |> parse
//(a::T,expr) where T <: AbstractFloat = //(RExpr(a),RExpr(expr)) |> parse
//(expr::ExprSymbol,b::T) where T <: AbstractFloat = //(RExpr(expr),RExpr(b)) |> parse
//(a::T,expr::ExprSymbol) where T <: AbstractFloat = //(RExpr(a),RExpr(expr)) |> parse
function //(a::T,b::T) where T <: AbstractFloat
    isnan(a) | isnan(b) | (isinf(a) & isinf(b)) && return NaN
    return //(RExpr(a),RExpr(b)) |> parse |> eval
end

inv(r) = Base.inv(r)
inv(r::T) where T <: MatExpr = r^-1

\(a,b) = Base.:\(a,b)
\(a::T,s::S) where T <: MatExpr where S <: Vector = (RExpr(a)^-1)*RExpr(s) |> parse |> mat
\(a::T,s::S) where T <: Vector where S <: MatExpr = (RExpr(a)^-1)*RExpr(s) |> parse |> mat

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
