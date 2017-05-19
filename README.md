# Reduce.jl

[![Build Status](https://travis-ci.org/chakravala/Reduce.jl.svg?branch=master)](https://travis-ci.org/chakravala/Reduce.jl) [![Coverage Status](https://coveralls.io/repos/github/chakravala/Reduce.jl/badge.svg?branch=master)](https://coveralls.io/github/chakravala/Reduce.jl?branch=master) [![codecov.io](http://codecov.io/github/chakravala/Reduce.jl/coverage.svg?branch=master)](http://codecov.io/github/chakravala/Reduce.jl?branch=master)

Interface for applying symbolic manipulation on [Julia expressions](https://docs.julialang.org/en/latest/manual/metaprogramming) using [REDUCE](http://www.reduce-algebra.com/index.htm)'s term rewrite system. The `Reduce` package currently provides the base functionality to work with Julia and Reduce expressions, provided that you have `redpsl` in your path. On GNU/Linux, `Pkg.build("Reduce")` will automatically download a precompiled binary of `redpsl` for you. If you are running a different Unix operating system, `Pkg.build("Reduce")` will download the source and attempt to compile `redpsl` for you. Automatic windows build not currently supported.

```Julia
julia> Pkg.add("Reduce"); Pkg.build("Reduce")
julia> using Reduce
Reduce (Free PSL version, revision 4015),  5-May-2017 ...
```
Similar to the [`Maxima.jl`](https://github.com/nsmith5/Maxima.jl) package, use `rcall` to evaluate Julia expressions or strings of reduce expressions using the PSL version of REDUCE.
```Julia
julia> rcall(:((1+π+x)^2))
:(π ^ 2 + 2 * π * x + 2π + x ^ 2 + 2x + 1)
```
With this package, expressions are piped into/from REDUCE and parsed using Julia's abstract syntax tree.
```Julia
julia> :(sin(x*im) + cos(y*ϕ)) |> rcall
:(cos((sqrt(5) * y + y) / 2) + sinh(x) * im)

julia> Meta.show_sexpr(ans)
(:call, :+, (:call, :cos, (:call, :/, (:call, :+, (:call, :*, (:call, :sqrt, 5), :y), :y), 2)), (:call, :*, (:call, :sinh, :x), :im))
```
In `IJulia` the output of `RExpr` will be displayed using LaTeX with the `rlfi` REDUCE package in `latex` mode; while in the REPL, the default `nat` output mode of REDUCE will be displayed. The output of `rcall` will be the same as its input type.
```Julia
julia> "int(sin(y)^2, y)" |> rcall
"( - cos(y)*sin(y) + y)/2"
```
Sequences of `Reduce` statements are automatically parsed into Julia `quote` blocks.
```Julia
julia> :((x+1+π)^2; int(1/(1+x^3),x)) |> RExpr

  2                    2
pi  + 2*pi*x + 2*pi + x  + 2*x + 1


                 2*x - 1          2
 2*sqrt(3)*atan(---------) - log(x  - x + 1) + 2*log(x + 1)
                 sqrt(3)
------------------------------------------------------------
                             6

julia> ans.str
2-element Array{String,1}:
 "pi**2 + 2*pi*x + 2*pi + x**2 + 2*x + 1"
 "(2*sqrt(3)*atan((2*x - 1)/sqrt(3)) - log(x**2 - x + 1) + 2*log(x + 1))/6"

julia> ans |> RExpr |> parse
quote
    π ^ 2 + 2 * π * x + 2π + x ^ 2 + 2x + 1
    ((2 * sqrt(3) * atan((2x - 1) / sqrt(3)) - log((x ^ 2 - x) + 1)) + 2 * log(x + 1)) / 6
end
```
Similar to `?` and `;`, `Reduce` provides a `reduce>` REPL using `}`.
```Julia
reduce> df(atan(golden_ratio*x),x);

          2              2
 sqrt(5)*x  + sqrt(5) - x  + 1
-------------------------------
           4      2
       2*(x  + 3*x  + 1)
```

The `Reduce` and `Maxima` packages can be imported and used simultaneously in Julia.
