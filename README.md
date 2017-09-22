# Reduce.jl

[![Build Status](https://travis-ci.org/chakravala/Reduce.jl.svg?branch=master)](https://travis-ci.org/chakravala/Reduce.jl) [![Build status](https://ci.appveyor.com/api/projects/status/kaqu2yri4vxyr63n?svg=true)](https://ci.appveyor.com/project/chakravala/reduce-jl) [![Coverage Status](https://coveralls.io/repos/github/chakravala/Reduce.jl/badge.svg?branch=master)](https://coveralls.io/github/chakravala/Reduce.jl?branch=master) [![codecov.io](http://codecov.io/github/chakravala/Reduce.jl/coverage.svg?branch=master)](http://codecov.io/github/chakravala/Reduce.jl?branch=master)
[![](https://img.shields.io/badge/docs-stable-blue.svg)](https://chakravala.github.io/Reduce.jl/stable)
[![](https://img.shields.io/badge/docs-latest-blue.svg)](https://chakravala.github.io/Reduce.jl/latest)

Interface for applying symbolic manipulation on [Julia expressions](https://docs.julialang.org/en/latest/manual/metaprogramming) using [REDUCE](http://www.reduce-algebra.com)'s term rewrite system. The `Reduce` package currently provides the base functionality to work with Julia and Reduce expressions, provided that you have `redpsl` in your path. On GNU/Linux/OSX/Windows, `Pkg.build("Reduce")` will automatically download a precompiled binary of `redpsl` for you. If you are running a different Unix operating system, the build script will download the source and attempt to compile `redpsl` for you, success depends on the build tools installed. Automatic Windows build is now supported.

```Julia
julia> Pkg.add("Reduce"); Pkg.build("Reduce")
julia> using Reduce
Reduce (Free PSL version, revision 4015),  5-May-2017 ...
```
The `Reduce` package currently provides a robust interface to directly use the PSL version of REDUCE within the Julia language and the REPL. This is achieved by interfacing `Expr` objects and `RExpr` objects using streams of characters.

> REDUCE is a system for doing scalar, vector and matrix algebra by computer, which also supports arbitrary precision numerical approximation and interfaces to gnuplot to provide graphics. It can be used interactively for simple calculations but also provides a full programming language, with a syntax similar to other modern programming languages.
> REDUCE has a long and distinguished place in the history of computer algebra systems. Other systems that address some of the same issues but sometimes with rather different emphasis are Axiom, Macsyma (Maxima), Maple and Mathematica.
> REDUCE is implemented in Lisp (as are Axiom and Macsyma), but this is completely hidden from the casual user. REDUCE primarily runs on either Portable Standard Lisp (PSL) or Codemist Standard Lisp (CSL), both of which are included in the SourceForge distribution. PSL is long-established and compiles to machine code, whereas CSL is newer and compiles to byte code. Hence, PSL may be faster but CSL may be available on a wider range of platforms.

Upcoming releases of `Reduce.jl` will integrate various REDUCE functionality and packages into the Julia language.

Similar to the [`Maxima.jl`](https://github.com/nsmith5/Maxima.jl) package, use `rcall` to evaluate Julia expressions or strings of reduce expressions using the PSL version of REDUCE.
```Julia
julia> :(int(sin(im*x+pi)^2-1,x)) |> rcall
:(((-(e ^ (4x)) - 4 * e ^ (2x) * x) + 1) / (8 * e ^ (2x)))
```
With this package, expressions are piped into/from REDUCE and parsed using Julia's abstract syntax tree.
```Julia
julia> :(sin(x*im) + cos(y*φ)) |> rcall
:(cos((sqrt(5) * y + y) / 2) + sinh(x) * im)

julia> Meta.show_sexpr(ans)
(:call, :+, (:call, :cos, (:call, :/, (:call, :+, (:call, :*, (:call, :sqrt, 5), :y), :y), 2)), (:call, :*, (:call, :sinh, :x), :im))
```
The output of `rcall` will be the same as its input type.
```Julia
julia> "int(sin(y)^2, y)" |> rcall
"( - cos(y)*sin(y) + y)/2"
```
In `IJulia` the display output of `RExpr` objects will be displayed using LaTeX with the `rlfi` REDUCE package in `latex` mode; while in the REPL, the default `nat` output mode of REDUCE will be displayed.

Sequences of `Reduce` statements are automatically parsed into Julia `quote` blocks using the `RExpr` constructor, which can `parse` back into a Julia expression.
```Julia
julia> :((x+1+π)^2; int(1/(1+x^3),x)) |> RExpr
"begin **(+(x,1,pi),2); int(/(1,+(1,**(x,3))),x) end"

julia> ans.str
2-element Array{String,1}:
 "begin **(+(x,1,pi),2)"
 "int(/(1,+(1,**(x,3))),x) end"

julia> ans |> RExpr |> rcall |> parse
quote
    π ^ 2 + 2 * π * x + 2π + x ^ 2 + 2x + 1
    ((2 * sqrt(3) * atan((2x - 1) / sqrt(3)) - log((x ^ 2 - x) + 1)) + 2 * log(x + 1)) / 6
end
```
Call `split(::RExpr)` to create a new `RExpr` object with all `Reduce` expressions split into separate array elements.

Similar to `?` and `;` in Julia, `Reduce` provides a `reduce>` REPL by `}`.
```Julia
reduce> df(atan(golden_ratio*x),x);

          2              2
 sqrt(5)*x  + sqrt(5) - x  + 1
-------------------------------
           4      2
       2*(x  + 3*x  + 1)
```
To expand functionality deeper into the Julia abstract syntax tree, more features are going to be built into this package as time progresses.
```Julia
julia> Expr(:for,:(i=2:34),:(product(i))) |> rcall
:(@big_str "295232799039604140847618609643520000000")
```
Parsing of reduce expressions is still under development, but it's already possible to parse various combinations of recursively nested program statements, such as
```Julia
julia> R"procedure fun; begin; x; return begin; return x end; x; end" |> parse
:(function fun
        x
        return begin
                return x
            end
        x
    end)

julia> ans == parse(RExpr(ans))
true
```
If you are `using Reduce` in julia, there are now an additional 530 methods available in the dispatch table so that trigonometric and other unary mathematical functions can be applied directly to `Expr` and `RExpr` objects. Check [src/unary.jl](src/unary.jl) for a list of currently implemented symbols.
```Julia
julia> Expr(:function,:fun,:(y=e^x)) |> log
:(function fun
        y = x
    end)
```
The `Reduce` and `Maxima` packages can currently be imported and used simultaneously in Julia. Place `using Reduce` as first package to load in the `~/.juliarc.jl` startup file to ensure the REPL loads properly (when `using OhMyREPL`). Otherwise, if you are loading this package when Julia has already been started, load it after `OhMyREPL`.

If the `}` REPL is not appearing or the `Reduce.PSL` pipe is broken, the session can be restored by simply calling `Reduce.Reset()`, without requiring a restart of `julia` or reloading the package.
