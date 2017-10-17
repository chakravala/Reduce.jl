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
    :evallhseq
]

switchtex = [
    :nat,
    :latex
]

Expr(:toplevel,[:(import Base: $i) for i ∈ switchbas]...) |> eval
:(export $([switchbas;switches;switchtex]...)) |> eval

for fun in [switchbas;switches;switchtex]
    parsegen(fun,:switch) |> eval
end

for fun in [switchbas;switches]
    for T in [:(Compat.String),:Expr]
        quote
            function $fun(expr::$T;be=0)
                convert($T, $fun(RExpr(expr);be=be))
            end
        end |> eval
    end
end

export @latex, @nat

for fun in switchtex
    for T in [:(Compat.String),:Expr]
        quote
            function $fun(expr::$T;be=0)
                convert(String, $fun(RExpr(expr);be=be))
            end
        end |> eval
    end
    quote
        macro $fun(expr)
            $fun(expr)
        end
    end
end
