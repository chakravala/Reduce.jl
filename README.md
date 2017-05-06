# Reduce

Interface for [reduce algebra](http://www.reduce-algebra.com/index.htm) for symbolic manipulation of [Julia expressions](https://docs.julialang.org/en/latest/manual/metaprogramming) using reduce algebra's term rewrite system.

```Julia
julia> Pkg.clone("git://github.com/chakravala/Reduce.jl.git")
julia> using Reduce
Reduce (Free PSL version, revision 4015),  5-May-2017 ...
```
Similar to [Maxima.jl](https://github.com/nsmith5/Maxima.jl) package, use `rcall` to evaluate Julia expressions or strings of reduce expressions using the PSL version of REDUCE.

```Julia
julia> rcall(:((1+π)^2))
:(π ^ 2 + 2π + 1)

julia> rcall(:(sin(x*im) + cos(y*ϕ)))
:(cos((sqrt(5) * y + y) / 2) + sinh(x) * im)

julia> rcall("int(sin(y)^2, y)")
"( - cos(y)*sin(y) + y)/2"
```
Reduce.jl currently provides the base functionality to work with Julia and Reduce expressions, provided that you have `redpsl` in your path.


[![Build Status](https://travis-ci.org/chakravala/Reduce.jl.svg?branch=master)](https://travis-ci.org/chakravala/Reduce.jl) [![Coverage Status](https://coveralls.io/repos/chakravala/Reduce.jl/badge.svg?branch=master&service=github)](https://coveralls.io/github/chakravala/Reduce.jl?branch=master) [![codecov.io](http://codecov.io/github/chakravala/Reduce.jl/coverage.svg?branch=master)](http://codecov.io/github/chakravala/Reduce.jl?branch=master)
