#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

switchbas = Symbol[
    :expand,
    :complex
]

switches = [
    :factor,
    :expandlog,
    :combinelog,
    :precise,
    :combineexpt,
    :rounded,
    :evallhseq,
    :horner
]

switchtex = [
    :nat,
    :latex
]

Expr(:toplevel,[:(import Base: $i) for i ∈ switchbas]...) |> eval
:(export $([switchbas;switches;switchtex]...)) |> eval
:(export $(Symbol.("@",[switches;switchtex])...)) |> eval

for fun in [switchbas;switches;switchtex]
    parsegen(fun,:switch) |> eval
end

for fun in [switchbas;switches]
    unfoldgen(fun,:switch) |> eval
    @eval begin
        function $fun(expr::Compat.String;be=0)
            convert(Compat.String, $fun(RExpr(expr);be=be))
        end
    end
end

for fun in switchtex
    unfoldgen(fun,:switch) |> eval
    @eval begin
        function $fun(expr::Compat.String;be=0)
            convert(String, $fun(RExpr(expr);be=be))
        end
        macro $fun(expr)
            $fun(expr)
        end
    end
end

for fun in switches
    @eval begin
        macro $fun(expr)
            :($$(QuoteNode(fun))($(esc(expr))))
        end
        $fun(expr;be=0) = expr
    end
end

function countops(expr)
    c = 0
    if typeof(expr) == Expr
        if expr.head == :call
            c += 1
        end
        for arg ∈ expr.args
            c += countops(arg)
        end
    end
    return c
end

function expravg(expr)
    cs = 0
    s = 0.0
    cp = 0
    p = 0.0
    if typeof(expr) == Expr
        if expr.head == :call && expr.args[1] == :^ &&
            expr.args[3] |> typeof <: Number
            cp += 1
            p  += abs(expr.args[3])
            (cst,st,cpt,pt) = expravg(expr.args[2])
            cs += cst
            s  += cst*st
            cp += cpt
            p  += cpt*pt
        else
            for arg ∈ expr.args
                (cst,st,cpt,pt) = expravg(arg)
                cs += cst
                s  += cst*st
                cp += cpt
                p  += cpt*pt
            end
        end
    elseif typeof(expr) <: Number
        cs += 1
        s  += log(abs(expr))
    end
    return (cs,cs == 0 ? 0.0 : s/cs,cp,cp == 0 ? 1.0 : p/cp)
end

function exprval(expr)
    val = expravg(expr)
    val[2]*val[4]*countops(expr)
end

export optimal

function optimal(expr)
    f = factor(expr)
    h = horner(expr)
    if exprval(h) ≤ exprval(f)
        return h
    else
        return f
    end
end

function sub(T::DataType,ixpr)
    if typeof(ixpr) == Expr
        expr = deepcopy(ixpr)
        if expr.head == :call && expr.args[1] == :^
            expr.args[2] = sub(T,expr.args[2])
            if typeof(expr.args[3]) == Expr
                expr.args[3] = sub(T,expr.args[3])
            end
        elseif expr.head == :macrocall &&
                expr.args[1] ∈ [Symbol("@int128_str"), Symbol("@big_str")]
            return convert(T,eval(expr))
        else
            for a ∈ 1:length(expr.args)
                expr.args[a] = sub(T,expr.args[a])
            end
        end
        return expr
    elseif typeof(ixpr) <: Number
        return convert(T,ixpr)
    end
    return ixpr
end

