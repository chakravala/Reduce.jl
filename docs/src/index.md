# Reduce.jl

*Symbolic parser generator for Julia language expressions using REDUCE algebra term rewrite system*


```@contents
```

## Introduction

REDUCE is a system for general algebraic computations of interest to mathematicians, scientists and engineers:

* exact arithmetic using integers and fractions;
* arbitrary precision numerical approximation;
* polynomial and rational function algebra;
* factorization and expansion of polynomials and rational functions;
* differentiation and integration of multi-variable functions;
* exponential, logarithmic, trigonometric and hyperbolic;
* output of results in a variety of formats;
* automatic and user controlled simplification of expressions;
* substitutions and pattern matching of expressions;
* quantifier elimination and decision for interpreted first-order logic;
* solution of ordinary differential equations;
* calculations with a wide variety of special (higher transcendental) functions;
* calculations involving matrices with numerical and symbolic elements;
* general matrix and non-commutative algebra;
* powerful intuitive user-level programming language;
* generating optimized numerical programs from symbolic input;
* Dirac matrix calculations of interest to high energy physicists;
* solution of single and simultaneous equations.

Interface for applying symbolic manipulation on [Julia expressions](https://docs.julialang.org/en/latest/manual/metaprogramming) using [REDUCE](http://www.reduce-algebra.com)'s term rewrite system:

* reduce expressions are `RExpr` objects that can `parse` into julia `Expr` objects and vice versa;
* interface link communicates and interprets via various reduce output modes using `rcall` method;
* high-level reduce-julia syntax parser-generator walks arbitrary expression to rewrite mathematical code;
* import operators from REDUCE using code generation to apply to arbitrary computational expressions;
* interactive `reduce>` REPL within the Julia terminal window activated by `}` key.

## Setup

The `Reduce` package currently provides the base functionality to work with Julia and Reduce expressions, provided that you have `redpsl` in your path. On GNU/Linux/OSX/Windows, `Pkg.build("Reduce")` will automatically download a precompiled binary of `redpsl` for you. If you are running a different Unix operating system, the build script will download the source and attempt to compile `redpsl` for you, success depends on the build tools installed. Automatic download on Windows is supported, although any **appveyor** build tests for Windows will fail due to absent software distribution infrastructure. However, the automated testing for **Travis CI** using Linux and OSX are fully operational `using Reduce`.

```Julia
julia> Pkg.add("Reduce"); Pkg.build("Reduce")
julia> using Reduce
Reduce (Free PSL version, revision 4015),  5-May-2017 ...
```
In order to support Unicode / UTF8 characters, the CSL version of reduce is required. The automated build script currently only fetches the PSL version. However, if you have `redcsl` installed on your system it can be used by Reduce.jl by setting the environment variable `ENV["REDUCE"] = "redcsl -w"` in julia.

For users who wish to experiment with precomplation, it is possible to enable extra precompilation scripts by setting the environment variable `ENV["REDPRE"] = "1"` in julia (only effective when `Reduce` is being compiled).

View the documentation [stable](https://chakravala.github.io/Reduce.jl/stable) / [latest](https://chakravala.github.io/Reduce.jl/latest) for more features and examples.

## Background

The `Reduce` package currently provides a robust interface to directly use the PSL version of REDUCE within the Julia language and the REPL. This is achieved by interfacing the abstract syntax tree of `Expr` objects with the parser generator for `RExpr` objects and then using an `IOBuffer` to communicate with `redpsl`.

> REDUCE is a system for doing scalar, vector and matrix algebra by computer, which also supports arbitrary precision numerical approximation and interfaces to gnuplot to provide graphics. It can be used interactively for simple calculations but also provides a full programming language, with a syntax similar to other modern programming languages.
> REDUCE has a long and distinguished place in the history of computer algebra systems. Other systems that address some of the same issues but sometimes with rather different emphasis are Axiom, Macsyma (Maxima), Maple and Mathematica.
> REDUCE is implemented in Lisp (as are Axiom and Macsyma), but this is completely hidden from the casual user. REDUCE primarily runs on either Portable Standard Lisp (PSL) or Codemist Standard Lisp (CSL), both of which are included in the SourceForge distribution. PSL is long-established and compiles to machine code, whereas CSL is newer and compiles to byte code. Hence, PSL may be faster but CSL may be available on a wider range of platforms.

Releases of `Reduce.jl` enable the general application of various REDUCE functionality and packages to manipulate the Julia language to simplify and compute new program expressions at run-time. Intended for uses where a symbolic pre-computation is required for numerical algorithm code generation.

> Julia is a high-level, high-performance dynamic programming language for numerical computing. It provides a sophisticated compiler, distributed parallel execution, numerical accuracy, and an extensive mathematical function library. Julia’s Base library, largely written in Julia itself, also integrates mature, best-of-breed open source C and Fortran libraries for linear algebra, random number generation, signal processing, and string processing.
> The strongest legacy of Lisp in the Julia language is its metaprogramming support. Like Lisp, Julia represents its own code as a data structure of the language itself. Since code is represented by objects that can be created and manipulated from within the language, it is possible for a program to transform and generate its own code. This allows sophisticated code generation without extra build steps, and also allows true Lisp-style macros operating at the level of abstract syntax trees.

## Usage

Reduce expressions encapsulated into `RExpr` objects can be manipulated within julia using the standard syntax. Create an expression object either using the `RExpr("expression")` string constructor or `R"expression"`. Additionally, arbitrary julia expressions can also be parsed directly using the `RExpr(expr)` constructor. Internally `RExpr` objects are represented as an array that can be accessed by calling `*.str[n]` on the object.

Sequences of reduce statements are automatically parsed into julia `quote` blocks using the `RExpr` constructor, which can `parse` back into a julia expression.
```Julia
julia> :((x+im+π)^2; int(1/(1+x^3),x)) |> RExpr
^(+(x,i,pi),2);
int(/(1,+(1,^(x,3))),x);

julia> rcall(ans,:expand) |> parse
quote
    (((π + 2x) * π + x ^ 2) - 1) + 2 * (π + x) * im
    ((2 * sqrt(3) * atan((2x - 1) // sqrt(3)) - log((x ^ 2 - x) + 1)) + 2 * log(x + 1)) // 6
end
```
Call `split(::RExpr)` to create a new `RExpr` object with all expressions split into separate array elements.

The `rcall` method is used to evaluate any type of expression.
```Julia
julia> :(int(sin(im*x+pi)^2-1,x)) |> rcall
:(-(((e ^ (4x) + 4 * e ^ (2x) * x) - 1)) // (8 * e ^ (2x)))
```
The output of `rcall` will be the same as its input type.
```Julia
julia> "int(sin(y)^2, y)" |> rcall
"( - cos(y)*sin(y) + y)/2"
```
Use `rcall(expr,switches...)` to evaluate `expr` using REDUCE mode `switches` like `:expand`, `:factor`, and `:latex`.

Mathematical operators and REDUCE modes can be applied directly to `Expr` and `RExpr` objects.
```Julia
julia> Expr(:function,:(fun(a,b)),:(return a^3+3*a^2*b+3*a*b^2+b^3)) |> factor
:(function fun(a, b)
        return (a + b) ^ 3
    end)
```
Although not all language features have been implemented yet, it is possible to directly execute a variety of REDUCE style input programs using a synergy of julia syntax.
```Julia
julia> Expr(:for,:(i=2:34),:(product(i))) |> rcall
:(@big_str "295232799039604140847618609643520000000")
```

### Output mode
Various output modes are supported. While in the REPL, the default `nat` output mode will be displayed for `RExpr` objects.
```Julia
julia> :(sin(x*im) + cos(y*φ)) |> RExpr

      (sqrt(5) + 1)*y
 cos(-----------------) + sinh(x)*i
             2
```
This same output can also be printed to the screen by calling `print(nat(r))` method.

It is possible to direclty convert a julia expression object to LaTeX code using the `latex` method.
```Julia
julia> print(@latex sin(x*im) + cos(y*φ))
\begin{displaymath}
\cos \left(\left(\left(\sqrt {5}+1\right) y\right)/2\right)+\sinh \,x\: i
\end{displaymath}
```
Internally, this command essentially expands to `rcall(:(sin(x*im) + cos(y*φ)),:latex) |> print`, which is equivalent.

![latex-equation](https://latex.codecogs.com/svg.latex?\cos&space;\left(\left(\left(\sqrt&space;{5}&plus;1\right)&space;y\right)/2\right)&plus;\sinh&space;x\:&space;i)

In `IJulia` the display output of `RExpr` objects will be rendered LaTeX with the `rlfi` REDUCE package in `latex` mode.

### REPL interface
Similar to `?` help and `;` shell modes in Julia, `Reduce` provides a `reduce>` REPL mode by pressing the `}` key as the first character in the julia terminal repl. The output is in `nat` mode.
```Julia
reduce> df(atan(golden_ratio*x),x);

           2              2
  sqrt(5)*x  + sqrt(5) - x  + 1
 -------------------------------
            4      2
        2*(x  + 3*x  + 1)
```

## Troubleshooting

If the `reduce>` REPL is not appearing when `}` is pressed or the `Reduce.PSL` pipe is broken, the session can be restored by simply calling `Reduce.Reset()`, without requiring a restart of `julia` or reloading the package. This kills the currently running `redpsl` session and then re-initializes it for new use.

### OhMyREPL Compatibility

Reduce.jl is compatible with the [OhMyREPL.jl](https://github.com/KristofferC/OhMyREPL.jl) package.

Place `using Reduce` as first package to load in the `~/.juliarc.jl` startup file to ensure the REPL loads properly (when also `using OhMyREPL`). Otherwise, if you are loading this package when Julia has already been started, load it after `OhMyREPL`.
