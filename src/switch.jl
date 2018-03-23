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
    end
end

for fun in [switches;switchtex]
    @eval begin
        macro $fun(expr)
            :($$(QuoteNode(fun))($(esc(expr))))
        end
    end
end
