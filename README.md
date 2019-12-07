*Symbolic parser generator for Julia language expressions using REDUCE algebra term rewriter*

<p align="center">
  <img src="./dev/assets/logo.png" alt="Reduce.jl"/>
</p>

[![Build Status](https://travis-ci.org/chakravala/Reduce.jl.svg?branch=master)](https://travis-ci.org/chakravala/Reduce.jl)
[![Build status](https://ci.appveyor.com/api/projects/status/kaqu2yri4vxyr63n?svg=true)](https://ci.appveyor.com/project/chakravala/reduce-jl)
[![Coverage Status](https://coveralls.io/repos/github/chakravala/Reduce.jl/badge.svg?branch=master)](https://coveralls.io/github/chakravala/Reduce.jl?branch=master)
[![codecov.io](http://codecov.io/github/chakravala/Reduce.jl/coverage.svg?branch=master)](http://codecov.io/github/chakravala/Reduce.jl?branch=master)
[![Docs Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://reduce.crucialflow.com/stable)
[![Docs Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://reduce.crucialflow.com/dev)
[![Join the chat at gitter](https://badges.gitter.im/Reduce-jl/Lobby.svg)](https://gitter.im/Reduce-jl/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Liberapay patrons](https://img.shields.io/liberapay/patrons/chakravala.svg)](https://liberapay.com/chakravala)

The premise behind [Reduce.jl](https://github.com/chakravala/Reduce.jl) is based on the idea that `Symbol` and `Expr` types can be translated into computer algebra rewrite commands and then automatically parsed back into Julia ASTs, essentially extending the Julia language into a fully programable symbolic AST rewrite environment.

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

This package is a heavily modifed version of Nathan Smith's [Maxima.jl](https://github.com/nsmith5/Maxima.jl) with many additional features.

## Background

The `Reduce` package currently provides a robust interface to directly use the CSL version of REDUCE within the Julia language and the REPL. This is achieved by interfacing the abstract syntax tree of `Expr` objects with the parser generator for `RExpr` objects and then using an `IOBuffer` to communicate with `redpsl`.

> REDUCE is a system for doing scalar, vector and matrix algebra by computer, which also supports arbitrary precision numerical approximation and interfaces to gnuplot to provide graphics. It can be used interactively for simple calculations but also provides a full programming language, with a syntax similar to other modern programming languages.
> REDUCE has a long and distinguished place in the history of computer algebra systems. Other systems that address some of the same issues but sometimes with rather different emphasis are Axiom, Macsyma (Maxima), Maple and Mathematica.
> REDUCE is implemented in Lisp (as are Axiom and Macsyma), but this is completely hidden from the casual user. REDUCE primarily runs on either Portable Standard Lisp (PSL) or Codemist Standard Lisp (CSL), both of which are included in the SourceForge distribution. PSL is long-established and compiles to machine code, whereas CSL is newer and compiles to byte code. Hence, PSL may be faster but CSL may be available on a wider range of platforms.

Releases of `Reduce.jl` enable the general application of various REDUCE functionality and packages to manipulate the Julia language to simplify and compute new program expressions at run-time. Intended for uses where a symbolic pre-computation is required for numerical algorithm code generation.

> Julia is a high-level, high-performance dynamic programming language for numerical computing. It provides a sophisticated compiler, distributed parallel execution, numerical accuracy, and an extensive mathematical function library. Julia’s Base library, largely written in Julia itself, also integrates mature, best-of-breed open source C and Fortran libraries for linear algebra, random number generation, signal processing, and string processing.
> The strongest legacy of Lisp in the Julia language is its metaprogramming support. Like Lisp, Julia represents its own code as a data structure of the language itself. Since code is represented by objects that can be created and manipulated from within the language, it is possible for a program to transform and generate its own code. This allows sophisticated code generation without extra build steps, and also allows true Lisp-style macros operating at the level of abstract syntax trees.
