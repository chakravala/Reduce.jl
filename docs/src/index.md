# Reduce.jl

*Symbolic parser generator for Julia language expressions using REDUCE algebra term rewriter*


```@contents
Pages = ["index.md","library.md","docs.md"]
```

## Introduction

The premise behind Reduce.jl is based on the idea that `Symbol` and `Expr` types can be translated into computer algebra rewrite commands and then automatically parsed back into Julia ASTs, essentially extending the Julia language into a fully programable symbolic AST rewrite environment.

REDUCE is a system for general algebraic computations of interest to mathematicians, scientists and engineers:

* exact arithmetic using integers and fractions; arbitrary precision numerical approximation;
* polynomial and rational function algebra; factorization and expansion of polynomials and rational functions;
* differentiation and integration of multi-variable functions; exponential, logarithmic, trigonometric and hyperbolic;
* output of results in a variety of formats; automatic and user controlled simplification of expressions;
* substitutions and pattern matching of expressions; quantifier elimination and decision for interpreted first-order logic;
* solution of ordinary differential equations; calculations with a wide variety of special (higher transcendental) functions;
* calculations involving matrices with numerical and symbolic elements; general matrix and non-commutative algebra;
* powerful intuitive user-level programming language; generating optimized numerical programs from symbolic input;
* Dirac matrix calculations of interest to high energy physicists; solution of single and simultaneous equations.

Interface for applying symbolic manipulation on [Julia expressions](https://docs.julialang.org/en/latest/manual/metaprogramming) using [REDUCE](http://www.reduce-algebra.com)'s term rewrite system:

* reduce expressions are `RExpr` objects that can `parse` into julia `Expr` objects and vice versa;
* interface link communicates and interprets via various reduce output modes using `rcall` method;
* high-level reduce-julia syntax parser-generator walks arbitrary expression to rewrite mathematical code;
* import operators from REDUCE using code generation to apply to arbitrary computational expressions;
* interactive `reduce>` REPL within the Julia terminal window activated by `}` key;
* extended arithmetic operators `+`,`-`,`*`,`^`,`/`,`//` compute on `Symbol` and `Expr` types;
* provides hundreds of internal and external methods each supporting many argument types.

Additional packages that depend on Reduce.jl are maintained at [JuliaReducePkg](https://github.com/JuliaReducePkg).

The upstream REDUCE software created by Anthony C. Hearn is maintained by collaborators on [SourceForge](https://sourceforge.net/p/reduce-algebra/).

## Setup

The `Reduce` package provides the base functionality to work with Julia and Reduce expressions, provided that you have `redcsl` in your path. On GNU/Linux/OSX/Windows, `Pkg.build("Reduce")` will automatically download a precompiled binary for you. If you are running a different Unix operating system, the build script will download the source and attempt to compile `redcsl` for you, success depends on the build tools installed. Automated testing for **Travis CI** and **appveyor** using Linux, OSX, and Windows are fully operational `using Reduce`.

```Julia
julia> Pkg.add("Reduce"); Pkg.build("Reduce")
julia> using Reduce
Reduce (Free CSL version, revision 4521),  11-March-2018 ...
```
For linux users who wish to speed up frequent precompilation, it is possible to disable extra precompilation scripts by setting the environment variable `ENV["REDPRE"] = "0"` in julia (only effective when `Reduce` is being compiled).

View the documentation [stable](https://chakravala.github.io/Reduce.jl/stable) / [latest](https://chakravala.github.io/Reduce.jl/latest) for more features and examples.

## Background

The `Reduce` package currently provides a robust interface to directly use the CSL version of REDUCE within the Julia language and the REPL. This is achieved by interfacing the abstract syntax tree of `Expr` objects with the parser generator for `RExpr` objects and then using an `IOBuffer` to communicate with `redpsl`.

> REDUCE is a system for doing scalar, vector and matrix algebra by computer, which also supports arbitrary precision numerical approximation and interfaces to gnuplot to provide graphics. It can be used interactively for simple calculations but also provides a full programming language, with a syntax similar to other modern programming languages.
> REDUCE has a long and distinguished place in the history of computer algebra systems. Other systems that address some of the same issues but sometimes with rather different emphasis are Axiom, Macsyma (Maxima), Maple and Mathematica.
> REDUCE is implemented in Lisp (as are Axiom and Macsyma), but this is completely hidden from the casual user. REDUCE primarily runs on either Portable Standard Lisp (PSL) or Codemist Standard Lisp (CSL), both of which are included in the SourceForge distribution. PSL is long-established and compiles to machine code, whereas CSL is newer and compiles to byte code. Hence, PSL may be faster but CSL may be available on a wider range of platforms.

Releases of `Reduce.jl` enable the general application of various REDUCE functionality and packages to manipulate the Julia language to simplify and compute new program expressions at run-time. Intended for uses where a symbolic pre-computation is required for numerical algorithm code generation.

> Julia is a high-level, high-performance dynamic programming language for numerical computing. It provides a sophisticated compiler, distributed parallel execution, numerical accuracy, and an extensive mathematical function library. Julia’s Base library, largely written in Julia itself, also integrates mature, best-of-breed open source C and Fortran libraries for linear algebra, random number generation, signal processing, and string processing.
> The strongest legacy of Lisp in the Julia language is its metaprogramming support. Like Lisp, Julia represents its own code as a data structure of the language itself. Since code is represented by objects that can be created and manipulated from within the language, it is possible for a program to transform and generate its own code. This allows sophisticated code generation without extra build steps, and also allows true Lisp-style macros operating at the level of abstract syntax trees.

## Usage

The extended algebraic symbolic expression mode of Reduce.jl is activated with [ForceImport.jl](https://github.com/chakravala/ForceImport.jl) by
```Julia
@force using Reduce.Algebra
```
This locally extends native Julia functions to `Symbol` and `Expr` types in the current module without extending global methods. Alternatively, the methods it provides can be accesed by prefixing `Algebra.` in front of the method.

Reduce expressions encapsulated into `RExpr` objects can be manipulated within julia using the standard syntax. Create an expression object either using the `RExpr("expression")` string constructor or `R"expression"`. Additionally, arbitrary julia expressions can also be parsed directly using the `RExpr(expr)` constructor. Internally `RExpr` objects are represented as an array that can be accessed by calling `*.str[n]` on the object.

When `Reduce` is used in Julia, standard arithmetic operations are now extended to also work on `Symbol` and `Expr` types.
```Julia
julia> 1-1/:n
:((n - 1) // n)

julia> ans^-:n
:(1 // ((n - 1) // n) ^ n)

julia> limit(ans,:n,Inf)
ℯ = 2.7182818284590...
```
Julia abstract syntax trees are automatically converted into sequences of reduce statements (using `RExpr` constructor) that are in return parsed into julia `quote` blocks usig `parse`.
The `rcall` method is used to evaluate any type of expression.
```Julia
julia> :(int(sin(im*x+pi)^2-1,x)) |> rcall
:((1 - (ℯ ^ (4x) + 4 * ℯ ^ (2x) * x)) // (8 * ℯ ^ (2x)))
```
However, there are often multiple equivalent ways of achieving the same result:
```Julia
julia> int(sin(im*:x+π)^2-1,:x)
:((1 - (ℯ ^ (4x) + 4 * ℯ ^ (2x) * x)) // (8 * ℯ ^ (2x)))
```
The output of `rcall` will be the same as its input type.
```Julia
julia> "int(sin(y)^2, y)" |> rcall
"( - cos(y)*sin(y) + y)/2"
```
Use `rcall(expr,switches...)` to evaluate `expr` using REDUCE mode `switches` like `:expand`, `:factor`, and `:latex`.
```Julia
julia> :((x+im+π)^2; int(1/(1+x^3),x)) |> RExpr
^(+(x,i,pi),2);
int(/(1,+(1,^(x,3))),x);

julia> rcall(ans,:horner) |> parse
quote
    ((π + 2x) * π + 2 * (π + x) * im + x ^ 2) - 1
    ((2 * sqrt(3) * atan((2x - 1) // sqrt(3)) - log((x ^ 2 - x) + 1)) + 2 * log(x + 1)) // 6
end
```
Mathematical operators and REDUCE modes can be applied directly to `Expr` and `RExpr` objects.
```Julia
julia> Expr(:function,:(fun(a,b)),:(return 4x^4-44x^3+61x^2+270x-525)) |> horner
:(function fun(a, b)
        return ((4 * (x - 11) * x + 61) * x + 270) * x - 525
    end)
```
Additionally, REDUCE switch statements can be used as macros to control evaluation of expressions.
```Julia
julia> @rounded @factor x^3-2x+1
:((x + 1.61803398875) * (x - 1) * (x - 0.61803398875))
```
Most core features have a corresponding Julia method, but language features that have not been implemented yet can also be directly evaluated with `rcall` using a synergy of julia syntax.
```Julia
julia> Expr(:for,:(i=2:34),:(product(i))) |> rcall
:(@big_str "295232799039604140847618609643520000000")
```
The `squash` function provides a way to reduce full program blocks into simplified functions, e.g.
```Julia
julia> Expr(:function,:(example(a,b)),quote
           z = 3
           target = z * :a * :b
           z -= 1
           target += z*(1-:a)*(1-:b)
       end) |> squash |> factor
:(function example(a, b)
        (5b - 2) * a - 2 * (b - 1)
    end)
```
where `z` is a program variable and `:a` and `:b` are symbolic variables.

### Output mode
 Various output modes are supported. While in the REPL, the default `nat` output mode will be displayed for `RExpr` objects.
```Julia
julia> :(sin(x*im) + cos(y*MathConstants.φ)) |> RExpr

     (sqrt(5) + 1)*y
cos(-----------------) + sinh(x)*i
            2
```
This same output can also be printed to the screen by calling `print(nat(r))` method.

It is possible to direclty convert a julia expression object to LaTeX code using the `latex` method.
```Julia
julia> print(@latex sin(x) + cos(y*MathConstants.φ))
\begin{displaymath}
\cos \left(\left(\left(\sqrt {5}+1\right) y\right)/2\right)+\sin \,x
\end{displaymath}
```
Internally, this command essentially expands to `rcall(:(sin(x) + cos(y*MathConstants.φ)),:latex) |> print`, which is equivalent.

In `IJulia` the display output of `RExpr` objects will be rendered LaTeX with the `rlfi` REDUCE package in `latex` mode.

### REPL interface
Similar to <kbd>?</kbd> help and <kbd>;</kbd> shell modes in Julia, `Reduce` provides a `reduce>` REPL mode by pressing <kbd>shift</kbd>+<kbd>]</kbd> as the first character in the julia terminal prompt. The output is in `nat` mode.
```Julia
reduce> df(atan(golden_ratio*x),x);

          2              2
 sqrt(5)*x  + sqrt(5) - x  + 1
-------------------------------
           4      2
       2*(x  + 3*x  + 1)
```

## Troubleshooting

If the `reduce>` REPL is not appearing when `}` is pressed or the Reduce pipe is broken, the session can be restored by simply calling `Reduce.Reset()`, without requiring a restart of `julia` or reloading the package. This kills the currently running Reduce session and then re-initializes it for new use.

Otherwise, questions can be asked on gitter/discourse or submit your issue or pull-request if you require additional features or noticed some unusual edge-case behavior.

### OhMyREPL Compatibility

Reduce.jl is compatible with the [OhMyREPL.jl](https://github.com/KristofferC/OhMyREPL.jl) package.

Place `using Reduce` as first package to load in the `~/.juliarc.jl` startup file to ensure the REPL loads properly (when also `using OhMyREPL`). Otherwise, if you are loading this package when Julia has already been started, load it after `OhMyREPL`.
