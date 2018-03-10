#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

calculus = Symbol[
    :df,
    :int
]

:(export $(calculus...)) |> eval

for fun in calculus
    parsegen(fun,:calculus) |> eval
    unfoldgen(fun,:calculus) |> eval
    quote
        function $fun(expr::Compat.String,s...;be=0)
            convert(Compat.String, $fun(RExpr(expr),s...;be=be))
        end 
    end |> eval
end
