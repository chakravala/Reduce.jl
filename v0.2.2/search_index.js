var documenterSearchIndex = {"docs": [

{
    "location": "index.html#",
    "page": "Home",
    "title": "Home",
    "category": "page",
    "text": ""
},

{
    "location": "index.html#Reduce.jl-1",
    "page": "Home",
    "title": "Reduce.jl",
    "category": "section",
    "text": "Symbolic parser generator for Julia language expressions using REDUCE algebra term rewrite system"
},

{
    "location": "index.html#Introduction-1",
    "page": "Home",
    "title": "Introduction",
    "category": "section",
    "text": "REDUCE is a system for general algebraic computations of interest to mathematicians, scientists and engineers:exact arithmetic using integers and fractions;\narbitrary precision numerical approximation;\npolynomial and rational function algebra;\nfactorization and expansion of polynomials and rational functions;\ndifferentiation and integration of multi-variable functions;\nexponential, logarithmic, trigonometric and hyperbolic;\noutput of results in a variety of formats;\nautomatic and user controlled simplification of expressions;\nsubstitutions and pattern matching of expressions;\nquantifier elimination and decision for interpreted first-order logic;\nsolution of ordinary differential equations;\ncalculations with a wide variety of special (higher transcendental) functions;\ncalculations involving matrices with numerical and symbolic elements;\ngeneral matrix and non-commutative algebra;\npowerful intuitive user-level programming language;\ngenerating optimized numerical programs from symbolic input;\nDirac matrix calculations of interest to high energy physicists;\nsolution of single and simultaneous equations.Interface for applying symbolic manipulation on Julia expressions using REDUCE's term rewrite system:reduce expressions are RExpr objects that can parse into julia Expr objects and vice versa;\ninterface link communicates and interprets via various reduce output modes using rcall method;\nhigh-level reduce-julia syntax parser-generator walks arbitrary expression to rewrite mathematical code;\nimport operators from REDUCE using code generation to apply to arbitrary computational expressions;\ninteractive reduce> REPL within the Julia terminal window activated by } key."
},

{
    "location": "index.html#Setup-1",
    "page": "Home",
    "title": "Setup",
    "category": "section",
    "text": "The Reduce package currently provides the base functionality to work with Julia and Reduce expressions, provided that you have redpsl in your path. On GNU/Linux/OSX/Windows, Pkg.build(\"Reduce\") will automatically download a precompiled binary of redpsl for you. If you are running a different Unix operating system, the build script will download the source and attempt to compile redpsl for you, success depends on the build tools installed. Automatic download on Windows is now supported.julia> Pkg.add(\"Reduce\"); Pkg.build(\"Reduce\")\njulia> using Reduce\nReduce (Free PSL version, revision 4015),  5-May-2017 ...In order to support Unicode / UTF8 characters, the CSL version of reduce is required. The automated build script currently only fetches the PSL version. However, if you have redcsl installed on your system it can be used by Reduce.jl by setting the environment variable ENV[\"REDUCE\"] = \"redcsl -w\" in julia.View the documentation stable / latest for more features and examples."
},

{
    "location": "index.html#Background-1",
    "page": "Home",
    "title": "Background",
    "category": "section",
    "text": "The Reduce package currently provides a robust interface to directly use the PSL version of REDUCE within the Julia language and the REPL. This is achieved by interfacing the abstract syntax tree of Expr objects with the parser generator for RExpr objects and then using an IOBuffer to communicate with redpsl.REDUCE is a system for doing scalar, vector and matrix algebra by computer, which also supports arbitrary precision numerical approximation and interfaces to gnuplot to provide graphics. It can be used interactively for simple calculations but also provides a full programming language, with a syntax similar to other modern programming languages. REDUCE has a long and distinguished place in the history of computer algebra systems. Other systems that address some of the same issues but sometimes with rather different emphasis are Axiom, Macsyma (Maxima), Maple and Mathematica. REDUCE is implemented in Lisp (as are Axiom and Macsyma), but this is completely hidden from the casual user. REDUCE primarily runs on either Portable Standard Lisp (PSL) or Codemist Standard Lisp (CSL), both of which are included in the SourceForge distribution. PSL is long-established and compiles to machine code, whereas CSL is newer and compiles to byte code. Hence, PSL may be faster but CSL may be available on a wider range of platforms.Releases of Reduce.jl enable the general application of various REDUCE functionality and packages to manipulate the Julia language to simplify and compute new program expressions at run-time. Intended for uses where a symbolic pre-computation is required for numerical algorithm code generation.Julia is a high-level, high-performance dynamic programming language for numerical computing. It provides a sophisticated compiler, distributed parallel execution, numerical accuracy, and an extensive mathematical function library. Julia’s Base library, largely written in Julia itself, also integrates mature, best-of-breed open source C and Fortran libraries for linear algebra, random number generation, signal processing, and string processing. The strongest legacy of Lisp in the Julia language is its metaprogramming support. Like Lisp, Julia represents its own code as a data structure of the language itself. Since code is represented by objects that can be created and manipulated from within the language, it is possible for a program to transform and generate its own code. This allows sophisticated code generation without extra build steps, and also allows true Lisp-style macros operating at the level of abstract syntax trees."
},

{
    "location": "index.html#Usage-1",
    "page": "Home",
    "title": "Usage",
    "category": "section",
    "text": "Reduce expressions encapsulated into RExpr objects can be manipulated within julia using the standard syntax. Create an expression object either using the RExpr(\"expression\") string constructor or R\"expression\". Additionally, arbitrary julia expressions can also be parsed directly using the RExpr(expr) constructor. Internally RExpr objects are represented as an array that can be accessed by calling *.str[n] on the object.Sequences of reduce statements are automatically parsed into julia quote blocks using the RExpr constructor, which can parse back into a julia expression.julia> :((x+im+π)^2; int(1/(1+x^3),x)) |> RExpr\n^(+(x,i,pi),2);\nint(/(1,+(1,^(x,3))),x);\n\njulia> rcall(ans,:expand) |> parse\nquote\n    (((π + 2x) * π + x ^ 2) - 1) + 2 * (π + x) * im\n    ((2 * sqrt(3) * atan((2x - 1) // sqrt(3)) - log((x ^ 2 - x) + 1)) + 2 * log(x + 1)) // 6\nendCall split(::RExpr) to create a new RExpr object with all expressions split into separate array elements.The rcall method is used to evaluate any type of expression.julia> :(int(sin(im*x+pi)^2-1,x)) |> rcall\n:(-(((e ^ (4x) + 4 * e ^ (2x) * x) - 1)) // (8 * e ^ (2x)))The output of rcall will be the same as its input type.julia> \"int(sin(y)^2, y)\" |> rcall\n\"( - cos(y)*sin(y) + y)/2\"Use rcall(expr,switches...) to evaluate expr using REDUCE mode switches like :expand, :factor, and :latex.Mathematical operators and REDUCE modes can be applied directly to Expr and RExpr objects.julia> Expr(:function,:(fun(a,b)),:(return a^3+3*a^2*b+3*a*b^2+b^3)) |> factor\n:(function fun(a, b)\n        return (a + b) ^ 3\n    end)Although not all language features have been implemented yet, it is possible to directly execute a variety of REDUCE style input programs using a synergy of julia syntax.julia> Expr(:for,:(i=2:34),:(product(i))) |> rcall\n:(@big_str \"295232799039604140847618609643520000000\")"
},

{
    "location": "index.html#Output-mode-1",
    "page": "Home",
    "title": "Output mode",
    "category": "section",
    "text": "Various output modes are supported. While in the REPL, the default nat output mode will be displayed for RExpr objects.julia> :(sin(x*im) + cos(y*φ)) |> RExpr\n\n      (sqrt(5) + 1)*y\n cos(-----------------) + sinh(x)*i\n             2This same output can also be printed to the screen by calling print(nat(r)) method.It is possible to direclty convert a julia expression object to LaTeX code using the latex method.julia> print(@latex sin(x*im) + cos(y*φ))\n\\begin{displaymath}\n\\cos \\left(\\left(\\left(\\sqrt {5}+1\\right) y\\right)/2\\right)+\\sinh \\,x\\: i\n\\end{displaymath}Internally, this command essentially expands to rcall(:(sin(x*im) + cos(y*φ)),:latex) |> print, which is equivalent.(Image: latex-equation)In IJulia the display output of RExpr objects will be rendered LaTeX with the rlfi REDUCE package in latex mode."
},

{
    "location": "index.html#REPL-interface-1",
    "page": "Home",
    "title": "REPL interface",
    "category": "section",
    "text": "Similar to ? help and ; shell modes in Julia, Reduce provides a reduce> REPL mode by pressing the } key as the first character in the julia terminal repl. The output is in nat mode.reduce> df(atan(golden_ratio*x),x);\n\n           2              2\n  sqrt(5)*x  + sqrt(5) - x  + 1\n -------------------------------\n            4      2\n        2*(x  + 3*x  + 1)"
},

{
    "location": "index.html#Troubleshooting-1",
    "page": "Home",
    "title": "Troubleshooting",
    "category": "section",
    "text": "If the reduce> REPL is not appearing when } is pressed or the Reduce.PSL pipe is broken, the session can be restored by simply calling Reduce.Reset(), without requiring a restart of julia or reloading the package. This kills the currently running redpsl session and then re-initializes it for new use."
},

{
    "location": "index.html#OhMyREPL-Compatibility-1",
    "page": "Home",
    "title": "OhMyREPL Compatibility",
    "category": "section",
    "text": "Reduce.jl is compatible with the OhMyREPL.jl package.Place using Reduce as first package to load in the ~/.juliarc.jl startup file to ensure the REPL loads properly (when also using OhMyREPL). Otherwise, if you are loading this package when Julia has already been started, load it after OhMyREPL."
},

{
    "location": "library.html#",
    "page": "Library",
    "title": "Library",
    "category": "page",
    "text": ""
},

{
    "location": "library.html#Reduce.jl-Library-1",
    "page": "Library",
    "title": "Reduce.jl Library",
    "category": "section",
    "text": ""
},

{
    "location": "library.html#Index-1",
    "page": "Library",
    "title": "Index",
    "category": "section",
    "text": ""
},

{
    "location": "library.html#Reduce.Reset",
    "page": "Library",
    "title": "Reduce.Reset",
    "category": "Function",
    "text": "Reduce.Reset()\n\nKills the REDUCE process and starts a new instance.\n\nExamples\n\njulia> Reduce.Reset()\nReduce (Free PSL version, revision 4015),  5-May-2017 ...\n\n\n\n"
},

{
    "location": "library.html#Reduce.RExpr",
    "page": "Library",
    "title": "Reduce.RExpr",
    "category": "Type",
    "text": "Reduce expression\n\nSummary:\n\ntype RExpr <: Any\n\nFields:\n\nstr::Array{Compat.String,1}\n\n\n\n"
},

{
    "location": "library.html#Reduce.rcall",
    "page": "Library",
    "title": "Reduce.rcall",
    "category": "Function",
    "text": "rcall(r::RExpr)\n\nEvaluate a Reduce expression.\n\nExamples\n\njulia> R\"int(sin(x), x)\" |> RExpr |> rcall\n - cos(x)\n\n\n\nrcall{T}(e::T)\n\nEvaluate a Julia expression or string using the Reduce interpretor and convert output back into the input type\n\nExamples\n\njulia> rcall(\"int(sin(y)^2, y)\")\n\"( - cos(y)*sin(y) + y)/2\"\n\njulia> rcall(:(int(1/(1+x^2), x)))\n:(atan(x))\n\n\n\n"
},

{
    "location": "library.html#Base.parse",
    "page": "Library",
    "title": "Base.parse",
    "category": "Function",
    "text": "Reduce.parse(r::RExpr)\n\nParse a Reduce expression into a Julia expression\n\nExamples\n\njulia> Reduce.parse(R\"sin(i*x)\")\n:(sin(im * x))\n\n\n\n"
},

{
    "location": "library.html#Reduce.load_package",
    "page": "Library",
    "title": "Reduce.load_package",
    "category": "Function",
    "text": "load_package(::Symbol)\n\nLoads the specified package into REDUCE\n\nExamples\n\njulia> load_package(:rlfi)\n\n\n\n"
},

{
    "location": "library.html#Reduce-Interface-1",
    "page": "Library",
    "title": "Reduce Interface",
    "category": "section",
    "text": "Reduce.ResetRExprrcallparseload_package"
},

{
    "location": "library.html#Imported-Operators-1",
    "page": "Library",
    "title": "Imported Operators",
    "category": "section",
    "text": "Reduce switch modes callable as functions from Julia:expand, :complex, :factor, :expandlog, :combinelog, :precise, :combineexpt, :rounded, :evallhseq, :nat, :latexCalculus operators:df, :intUnary operators:abs, :conj, :factorial, :floor, :max, :min, :round, :sign, :acos, :acosh, :acot, :acoth, :acsc, :acsch, :asec, :asech, :asin, :asinh, :atan, :atanh, :atan2, :cos, :cosh, :cot, :coth, :csc, :csch, :exp, :hypot, :log, :log10, :sec, :sech, :sin, :sinh, :sqrt, :tan, :tanh, :gamma, :factorize:beta, :besseli, :besselj, :besselk, :bessely, :polygamma, :zeta:ibeta, :igamma, :ln, :psi, :bernoulli, :continued_fraction, :ci, :dilog, :ei, :si, :airy_ai, :airy_aiprime, :airy_bi, :airy_biprime, :hanekl1, :hankel2, :kummerm, :kummeru, :lommel1, :lommel2, :struveh, :struvel, :whittakerm, :whittakeru, :solidharmonicy, :sphericalharmonicy:ceiling, :fix, :impart, :repart, :nextprime, :euler, :fibonacci, :motzkin, :random, :random_new_seed"
},

{
    "location": "library.html#Reduce.parsegen",
    "page": "Library",
    "title": "Reduce.parsegen",
    "category": "Function",
    "text": "parsegen(::Symbol,::Symbol)\n\nParser generator that outputs code to walk and manipulate REDUCE expressions\n\n\n\n"
},

{
    "location": "library.html#Reduce.linefilter",
    "page": "Library",
    "title": "Reduce.linefilter",
    "category": "Function",
    "text": "linefilter(::Expr)\n\nRecursively filters out :line blocks from Expr objects\n\n\n\n"
},

{
    "location": "library.html#Reduce.Rational",
    "page": "Library",
    "title": "Reduce.Rational",
    "category": "Function",
    "text": "Reduce.Rational(::Bool)\n\nToggle whether to use '/' or '//' for division in julia expressions\n\n\n\n"
},

{
    "location": "library.html#Reduce.SubCall",
    "page": "Library",
    "title": "Reduce.SubCall",
    "category": "Function",
    "text": "Reduce.SubCall(::Bool)\n\nToggle whether to substitute additional expressions\n\n\n\n"
},

{
    "location": "library.html#Tools-and-Options-1",
    "page": "Library",
    "title": "Tools & Options",
    "category": "section",
    "text": "Reduce.parsegenReduce.linefilterReduce.RationalReduce.SubCall"
},

]}
