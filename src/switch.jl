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
    quote
        function $fun(expr::Compat.String;be=0)
            convert(Compat.String, $fun(RExpr(expr);be=be))
        end
    end |> eval
    unfoldgen(fun,:switch) |> eval
end

export @latex, @nat

for fun in switchtex
    quote
        function $fun(expr::Compat.String;be=0)
            convert(String, $fun(RExpr(expr);be=be))
        end
    end |> eval
    unfoldgen(fun,:switch) |> eval
    quote
        macro $fun(expr)
            $fun(expr)
        end
    end
end
