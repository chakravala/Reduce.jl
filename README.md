# Reduce.jl

Interface for applying [reduce algebra](http://www.reduce-algebra.com/index.htm)'s symbolic manipulation on [Julia expressions](https://docs.julialang.org/en/latest/manual/metaprogramming) using REDUCE's term rewrite system.

```Julia
julia> Pkg.clone("git://github.com/chakravala/Reduce.jl.git")
julia> using Reduce
Reduce (Free PSL version, revision 4015),  5-May-2017 ...
```
Similar to [Maxima.jl](https://github.com/nsmith5/Maxima.jl) package, use `rcall` to evaluate Julia expressions or strings of reduce expressions using the PSL version of REDUCE. In `IJulia` the output of `RExpr` will be displayed using LaTeX.
```Julia
julia> rcall(:((1+π+x)^2))
:(π ^ 2 + 2 * π * x + 2π + x ^ 2 + 2x + 1)

julia> :(sin(x*im) + cos(y*ϕ)) |> rcall
:(cos((sqrt(5) * y + y) / 2) + sinh(x) * im)

julia> Meta.show_sexpr(ans)
(:call, :+, (:call, :cos, (:call, :/, (:call, :+, (:call, :*, (:call, :sqrt, 5), :y), :y), 2)), (:call, :*, (:call, :sinh, :x), :im))

julia> RExpr(:(sin(x*im) + cos(y*ϕ)))

     sqrt(5)*y + y
cos(---------------) + sinh(x)*i
           2

julia> "int(sin(y)^2, y)" |> rcall
"( - cos(y)*sin(y) + y)/2"

julia> :(int(1/(im-1+x^4),x)) |> RExpr

                                       1/4
        1/4                     (i - 1)   *sqrt(2) - 2*x
((i - 1)   *sqrt(2)*( - 2*atan(--------------------------)
                                          1/4
                                   (i - 1)   *sqrt(2)

                      1/4
               (i - 1)   *sqrt(2) + 2*x
     + 2*atan(--------------------------)
                         1/4
                  (i - 1)   *sqrt(2)

                     1/4                            2
     - log( - (i - 1)   *sqrt(2)*x + sqrt(i - 1) + x )

                  1/4                            2
     + log((i - 1)   *sqrt(2)*x + sqrt(i - 1) + x )))/(8*(i - 1))

julia> ans.str
"((i - 1)**(1/4)*sqrt(2)*( - 2*atan(((i - 1)**(1/4)*sqrt(2) - 2*x)/((i - 1)**(1/4)*sqrt(2))) + 2*atan(((i - 1)**(1/4)*sqrt(2) + 2*x)/((i - 1)**(1/4)*sqrt(2))) - log( - (i - 1)**(1/4)*sqrt(2)*x + sqrt(i - 1) + x**2) + log((i - 1)**(1/4)*sqrt(2)*x + sqrt(i - 1) + x**2)))/(8*(i - 1))"

julia> ans |> RExpr |> parse
:(((im - 1) ^ (1 / 4) * sqrt(2) * (((-2 * atan(((im - 1) ^ (1 / 4) * sqrt(2) - 2x) / ((im - 1) ^ (1 / 4) * sqrt(2))) + 2 * atan(((im - 1) ^ (1 / 4) * sqrt(2) + 2x) / ((im - 1) ^ (1 / 4) * sqrt(2)))) - log(-((im - 1) ^ (1 / 4)) * sqrt(2) * x + sqrt(im - 1) + x ^ 2)) + log((im - 1) ^ (1 / 4) * sqrt(2) * x + sqrt(im - 1) + x ^ 2))) / (8 * (im - 1)))

```
Reduce.jl currently provides the base functionality to work with Julia and Reduce expressions, provided that you have `redpsl` in your path.


[![Build Status](https://travis-ci.org/chakravala/Reduce.jl.svg?branch=master)](https://travis-ci.org/chakravala/Reduce.jl) [![Coverage Status](https://coveralls.io/repos/chakravala/Reduce.jl/badge.svg?branch=master&service=github)](https://coveralls.io/github/chakravala/Reduce.jl?branch=master) [![codecov.io](http://codecov.io/github/chakravala/Reduce.jl/coverage.svg?branch=master)](http://codecov.io/github/chakravala/Reduce.jl?branch=master)
