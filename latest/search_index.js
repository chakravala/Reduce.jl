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
    "text": "Symbolic parser generator for Julia language expressions using REDUCE algebra term rewriterPages = [\"index.md\",\"library.md\",\"docs.md\"]"
},

{
    "location": "index.html#Introduction-1",
    "page": "Home",
    "title": "Introduction",
    "category": "section",
    "text": "The premise behind Reduce.jl is based on the idea that Symbol and Expr types can be translated into computer algebra rewrite commands and then automatically parsed back into Julia ASTs, essentially extending the Julia language into a fully programable symbolic AST rewrite environment.REDUCE is a system for general algebraic computations of interest to mathematicians, scientists and engineers:exact arithmetic using integers and fractions; arbitrary precision numerical approximation;\npolynomial and rational function algebra; factorization and expansion of polynomials and rational functions;\ndifferentiation and integration of multi-variable functions; exponential, logarithmic, trigonometric and hyperbolic;\noutput of results in a variety of formats; automatic and user controlled simplification of expressions;\nsubstitutions and pattern matching of expressions; quantifier elimination and decision for interpreted first-order logic;\nsolution of ordinary differential equations; calculations with a wide variety of special (higher transcendental) functions;\ncalculations involving matrices with numerical and symbolic elements; general matrix and non-commutative algebra;\npowerful intuitive user-level programming language; generating optimized numerical programs from symbolic input;\nDirac matrix calculations of interest to high energy physicists; solution of single and simultaneous equations.Interface for applying symbolic manipulation on Julia expressions using REDUCE\'s term rewrite system:reduce expressions are RExpr objects that can parse into julia Expr objects and vice versa;\ninterface link communicates and interprets via various reduce output modes using rcall method;\nhigh-level reduce-julia syntax parser-generator walks arbitrary expression to rewrite mathematical code;\nimport operators from REDUCE using code generation to apply to arbitrary computational expressions;\ninteractive reduce> REPL within the Julia terminal window activated by } key;\nextended arithmetic operators +,-,*,^,/,// compute on Symbol and Expr types;\nprovides hundreds of internal and external methods each supporting many argument types.Additional packages that depend on Reduce.jl are maintained at JuliaReducePkg.The upstream REDUCE software created by Anthony C. Hearn is maintained by collaborators on SourceForge."
},

{
    "location": "index.html#Setup-1",
    "page": "Home",
    "title": "Setup",
    "category": "section",
    "text": "The Reduce package provides the base functionality to work with Julia and Reduce expressions, provided that you have redcsl in your path. On GNU/Linux/OSX/Windows, Pkg.build(\"Reduce\") will automatically download a precompiled binary for you. If you are running a different Unix operating system, the build script will download the source and attempt to compile redcsl for you, success depends on the build tools installed. Automated testing for Travis CI and appveyor using Linux, OSX, and Windows are fully operational using Reduce.julia> Pkg.add(\"Reduce\"); Pkg.build(\"Reduce\")\njulia> using Reduce\nReduce (Free CSL version, revision 4521),  11-March-2018 ...For users who wish to experiment with precomplation, it is possible to enable extra precompilation scripts by setting the environment variable ENV[\"REDPRE\"] = \"1\" in julia (only effective when Reduce is being compiled).View the documentation stable / latest for more features and examples."
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
    "text": "The extended algebraic symbolic expression mode of Reduce.jl is activated with ForceImport.jl by@force using Reduce.AlgebraThis locally extends native Julia functions to Symbol and Expr types in the current module without extending global methods. Alternatively, the methods it provides can be accesed by prefixing Algebra. in front of the method.Reduce expressions encapsulated into RExpr objects can be manipulated within julia using the standard syntax. Create an expression object either using the RExpr(\"expression\") string constructor or R\"expression\". Additionally, arbitrary julia expressions can also be parsed directly using the RExpr(expr) constructor. Internally RExpr objects are represented as an array that can be accessed by calling *.str[n] on the object.When Reduce is used in Julia, standard arithmetic operations are now extended to also work on Symbol and Expr types.julia> 1-1/:n\n:((n - 1) // n)\n\njulia> ans^-:n\n:(1 // ((n - 1) // n) ^ n)\n\njulia> limit(ans,:n,Inf)\ne = 2.7182818284590...Julia abstract syntax trees are automatically converted into sequences of reduce statements (using RExpr constructor) that are in return parsed into julia quote blocks usig parse. The rcall method is used to evaluate any type of expression.julia> :(int(sin(im*x+pi)^2-1,x)) |> rcall\n:((1 - (e ^ (4x) + 4 * e ^ (2x) * x)) // (8 * e ^ (2x)))However, there are often multiple equivalent ways of achieving the same result:julia> int(sin(im*:x+π)^2-1,:x)\n:((1 - (e ^ (4x) + 4 * e ^ (2x) * x)) // (8 * e ^ (2x)))The output of rcall will be the same as its input type.julia> \"int(sin(y)^2, y)\" |> rcall\n\"( - cos(y)*sin(y) + y)/2\"Use rcall(expr,switches...) to evaluate expr using REDUCE mode switches like :expand, :factor, and :latex.julia> :((x+im+π)^2; int(1/(1+x^3),x)) |> RExpr\n^(+(x,i,pi),2);\nint(/(1,+(1,^(x,3))),x);\n\njulia> rcall(ans,:horner) |> parse\nquote\n    ((π + 2x) * π + 2 * (π + x) * im + x ^ 2) - 1\n    ((2 * sqrt(3) * atan((2x - 1) // sqrt(3)) - log((x ^ 2 - x) + 1)) + 2 * log(x + 1)) // 6\nendMathematical operators and REDUCE modes can be applied directly to Expr and RExpr objects.julia> Expr(:function,:(fun(a,b)),:(return 4x^4-44x^3+61x^2+270x-525)) |> horner\n:(function fun(a, b)\n        return ((4 * (x - 11) * x + 61) * x + 270) * x - 525\n    end)Additionally, REDUCE switch statements can be used as macros to control evaluation of expressions.julia> @rounded @factor x^3-2x+1\n:((x + 1.61803398875) * (x - 1) * (x - 0.61803398875))Most core features have a corresponding Julia method, but language features that have not been implemented yet can also be directly evaluated with rcall using a synergy of julia syntax.julia> Expr(:for,:(i=2:34),:(product(i))) |> rcall\n:(@big_str \"295232799039604140847618609643520000000\")The squash function provides a way to reduce full program blocks into simplified functions, e.g.julia> Expr(:function,:(example(a,b)),quote\n           z = 3\n           target = z * :a * :b\n           z -= 1\n           target += z*(1-:a)*(1-:b)\n       end) |> squash |> factor\n:(function example(a, b)\n        (5b - 2) * a - 2 * (b - 1)\n    end)where z is a program variable and :a and :b are symbolic variables."
},

{
    "location": "index.html#Output-mode-1",
    "page": "Home",
    "title": "Output mode",
    "category": "section",
    "text": "Various output modes are supported. While in the REPL, the default nat output mode will be displayed for RExpr objects.julia> :(sin(x*im) + cos(y*φ)) |> RExpr\n\n     (sqrt(5) + 1)*y\ncos(-----------------) + sinh(x)*i\n            2This same output can also be printed to the screen by calling print(nat(r)) method.It is possible to direclty convert a julia expression object to LaTeX code using the latex method.julia> print(@latex sin(x) + cos(y*φ))\n\\begin{displaymath}\n\\cos \\left(\\left(\\left(\\sqrt {5}+1\\right) y\\right)/2\\right)+\\sin \\,x\n\\end{displaymath}Internally, this command essentially expands to rcall(:(sin(x) + cos(y*φ)),:latex) |> print, which is equivalent.In IJulia the display output of RExpr objects will be rendered LaTeX with the rlfi REDUCE package in latex mode."
},

{
    "location": "index.html#REPL-interface-1",
    "page": "Home",
    "title": "REPL interface",
    "category": "section",
    "text": "Similar to <kbd>?</kbd> help and <kbd>;</kbd> shell modes in Julia, Reduce provides a reduce> REPL mode by pressing <kbd>shift</kbd>+<kbd>]</kbd> as the first character in the julia terminal prompt. The output is in nat mode.reduce> df(atan(golden_ratio*x),x);\n\n          2              2\n sqrt(5)*x  + sqrt(5) - x  + 1\n-------------------------------\n           4      2\n       2*(x  + 3*x  + 1)"
},

{
    "location": "index.html#Troubleshooting-1",
    "page": "Home",
    "title": "Troubleshooting",
    "category": "section",
    "text": "If the reduce> REPL is not appearing when } is pressed or the Reduce pipe is broken, the session can be restored by simply calling Reduce.Reset(), without requiring a restart of julia or reloading the package. This kills the currently running Reduce session and then re-initializes it for new use.Otherwise, questions can be asked on gitter/discourse or submit your issue or pull-request if you require additional features or noticed some unusual edge-case behavior."
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
    "text": "Pages = [\"index.md\",\"library.md\",\"docs.md\"]"
},

{
    "location": "library.html#Index-1",
    "page": "Library",
    "title": "Index",
    "category": "section",
    "text": "Pages = [\"library.md\"]"
},

{
    "location": "library.html#Reduce.Reset",
    "page": "Library",
    "title": "Reduce.Reset",
    "category": "function",
    "text": "Reduce.Reset()\n\nKills the REDUCE process and starts a new instance.\n\nExamples\n\njulia> Reduce.Reset()\nReduce (Free PSL version, revision 4015),  5-May-2017 ...\n\n\n\n"
},

{
    "location": "library.html#Reduce.RExpr",
    "page": "Library",
    "title": "Reduce.RExpr",
    "category": "type",
    "text": "Reduce expression\n\nSummary:\n\ntype RExpr <: Any\n\nFields:\n\nstr::Array{Compat.String,1}\n\n\n\n"
},

{
    "location": "library.html#Reduce.rcall",
    "page": "Library",
    "title": "Reduce.rcall",
    "category": "function",
    "text": "rcall(r::RExpr)\n\nEvaluate a Reduce expression.\n\nExamples\n\njulia> R\"int(sin(x), x)\" |> RExpr |> rcall\n - cos(x)\n\n\n\nrcall{T}(e::T)\n\nEvaluate a Julia expression or string using the Reduce interpretor and convert output back into the input type\n\nExamples\n\njulia> rcall(\"int(sin(y)^2, y)\")\n\"( - cos(y)*sin(y) + y)/2\"\n\njulia> rcall(:(int(1/(1+x^2), x)))\n:(atan(x))\n\n\n\n"
},

{
    "location": "library.html#Base.parse",
    "page": "Library",
    "title": "Base.parse",
    "category": "function",
    "text": "Reduce.parse(r::RExpr)\n\nParse a Reduce expression into a Julia expression\n\nExamples\n\njulia> Reduce.parse(R\"sin(i*x)\")\n:(sin(im * x))\n\n\n\n"
},

{
    "location": "library.html#Reduce.load_package",
    "page": "Library",
    "title": "Reduce.load_package",
    "category": "function",
    "text": "load_package(::Symbol)\n\nLoads the specified package into REDUCE\n\nExamples\n\njulia> load_package(:rlfi)\n\n\n\n"
},

{
    "location": "library.html#Reduce.squash",
    "page": "Library",
    "title": "Reduce.squash",
    "category": "function",
    "text": "squash(expr)\n\nReduces an entire program statement block using symbolic rewriting\n\n\n\n"
},

{
    "location": "library.html#Reduce-Interface-1",
    "page": "Library",
    "title": "Reduce Interface",
    "category": "section",
    "text": "Reduce.ResetRExprrcallparseload_packagesquash"
},

{
    "location": "library.html#Imported-Operators-1",
    "page": "Library",
    "title": "Imported Operators",
    "category": "section",
    "text": "Reduce switch modes callable as functions from Juliaexpand, complex, factor, horner, expandlog, combinelog, precise, combineexpt, rounded, evallhseq, nat, latexReduce operators with multiple argumentsdf, int, limit, sum, prod, +, -, ^, *, /, //Unary operatorsabs, conj, factorial, floor, max, min, round, sign, acos, acosh, acot, acoth, acsc, acsch, asec, asech, asin, asinh, atan, atanh, atan2, cos, cosh, cot, coth, csc, csch, exp, hypot, log, log10, sec, sech, sin, sinh, sqrt, tan, tanh, gamma, factorizebeta, besseli, besselj, besselk, bessely, polygamma, zetaibeta, igamma, ln, psi, bernoulli, continued_fraction, ci, dilog, ei, si, airy_ai, airy_aiprime, airy_bi, airy_biprime, hanekl1, hankel2, kummerm, kummeru, lommel1, lommel2, struveh, struvel, whittakerm, whittakeru, solidharmonicy, sphericalharmonicyceiling, fix, impart, repart, nextprime, euler, fibonacci, motzkin, random, random_new_seed"
},

{
    "location": "library.html#Reduce.parsegen",
    "page": "Library",
    "title": "Reduce.parsegen",
    "category": "function",
    "text": "parsegen(::Symbol,::Symbol)\n\nParser generator that outputs code to walk and manipulate REDUCE expressions\n\n\n\n"
},

{
    "location": "library.html#Reduce.unfoldgen",
    "page": "Library",
    "title": "Reduce.unfoldgen",
    "category": "function",
    "text": "unfoldgen(::Symbol,::Symbol)\n\nParser generator that outputs code to walk and manipulate Julia expressions\n\n\n\n"
},

{
    "location": "library.html#Reduce.linefilter",
    "page": "Library",
    "title": "Reduce.linefilter",
    "category": "function",
    "text": "linefilter(::Expr)\n\nRecursively filters out :line blocks from Expr objects\n\n\n\n"
},

{
    "location": "library.html#Reduce.Rational",
    "page": "Library",
    "title": "Reduce.Rational",
    "category": "function",
    "text": "Reduce.Rational(::Bool)\n\nToggle whether to use \'/\' or \'//\' for division in julia expressions\n\n\n\n"
},

{
    "location": "library.html#Reduce.SubCall",
    "page": "Library",
    "title": "Reduce.SubCall",
    "category": "function",
    "text": "Reduce.SubCall(::Bool)\n\nToggle whether to substitute additional expressions\n\n\n\n"
},

{
    "location": "library.html#Reduce.SubHold",
    "page": "Library",
    "title": "Reduce.SubHold",
    "category": "function",
    "text": "Reduce.SubHold(::Real)\n\nSleep timer in case of clogged Reduce pipe on SubCall\n\n\n\n"
},

{
    "location": "library.html#Reduce.SubFail",
    "page": "Library",
    "title": "Reduce.SubFail",
    "category": "function",
    "text": "Reduce.SubFail(::Integer)\n\nFailure limit in case of clogged Reduce pipe on SubCall\n\n\n\n"
},

{
    "location": "library.html#Reduce.ColCheck",
    "page": "Library",
    "title": "Reduce.ColCheck",
    "category": "function",
    "text": "Reduce.ColCheck(::Bool)\n\nToggle whether to reset REPL linewidth on each show\n\n\n\n"
},

{
    "location": "library.html#Reduce.PrintLog",
    "page": "Library",
    "title": "Reduce.PrintLog",
    "category": "function",
    "text": "Reduce.PrintLog(::Bool)\n\nToggle whether to display the log of REDUCE commands\n\n\n\n"
},

{
    "location": "library.html#Tools-and-Options-1",
    "page": "Library",
    "title": "Tools & Options",
    "category": "section",
    "text": "Reduce.parsegenReduce.unfoldgenReduce.linefilterReduce.RationalReduce.SubCallReduce.SubHoldReduce.SubFailReduce.ColCheckReduce.PrintLog"
},

{
    "location": "man/acknowledgement.html#",
    "page": "Acknowledgement",
    "title": "Acknowledgement",
    "category": "page",
    "text": ""
},

{
    "location": "man/acknowledgement.html#Acknowledgement-1",
    "page": "Acknowledgement",
    "title": "Acknowledgement",
    "category": "section",
    "text": "This is the documentation for Reduce.jl. Part of this manual has been taken from the upstream REDUCE User\'s Manual and has been adapated for use with the Julia language.Copyright ©2004–2017 Anthony C. Hearn, Rainer Schöpf and contributors to the Reduce project. All rights reserved. Reproduction of this manual is allowed, provided that the source of the material is clearly acknowledged, and the copyright notice is retained.The production of this version of the manual has been the result of the contributions of a large number of individuals who have taken the time and effort to suggest improvements to previous versions, and to draft new sections. Particular thanks are due to Gerry Rayna, who provided a draft rewrite of most of the first half of the manual. Other people who have made significant contributions have included John Fitch, Martin Griss, Stan Kameny, Jed Marti, Herbert Melenk, Don Morrison, Arthur Norman, Eberhard Schrüfer, Larry Seward and Walter Tietze. Finally, Richard Hitt produced a TeX version of the REDUCE 3.3 manual, which has been a useful guide for the production of the LaTeX version of this manual."
},

{
    "location": "man/01-introduction.html#",
    "page": "1 Introductory Information",
    "title": "1 Introductory Information",
    "category": "page",
    "text": ""
},

{
    "location": "man/01-introduction.html#Introductory-Information-1",
    "page": "1 Introductory Information",
    "title": "1 Introductory Information",
    "category": "section",
    "text": "REDUCE is a system for carrying out algebraic operations accurately, no matter how complicated the expressions become. It can manipulate polynomials in a variety of forms, both expanding and factoring them, and extract various parts of them as required. REDUCE can also do differentiation and integration, but we shall only show trivial examples of this in this introduction. Other topics not considered include the use of arrays, the definition of procedures and operators, the specific routines for high energy physics calculations, the use of files to eliminate repetitious typing and for saving results, and the editing of the input text.Also not considered in any detail in this introduction are the many options that are available for varying computational procedures, output forms, number systems used, and so on.REDUCE is designed to be an interactive system, so that the user can input an algebraic expression and see its value before moving on to the next calculation. For those systems that do not support interactive use, or for those calculations, especially long ones, for which a standard script can be defined, REDUCE can also be used in batch mode. In this case, a sequence of commands can be given to REDUCE and results obtained without any user interaction during the computation.In this introduction, we shall limit ourselves to the interactive use of REDUCE, since this illustrates the capabilities of the system. However, keep  in mind that Reduce.jl also provides Julia methods that automatically parse these statements from/into Julia AST. When REDUCE is called, it begins by printing a banner message like:julia> using Reduce\nReduce (Free CSL version, revision 4521),  11-March-2018 ...where the version number and the system release date will change from time to time in deps/svn.jl. To enter the Reduce REPL, type <shift>+<]> as the first character in the Julia prompt.reduce>You can now type a REDUCE statement, terminated by a semicolon to indicate the end of the expression, for example:reduce> (x+y+z)^2;This expression would normally be followed by another character (a <Return> on an ASCII keyboard) to “wake up” the system, which would then input the expression, evaluate it, and return the result:           2\n(y + z + x)Note that in Julia the expand switch is disabled by default, unlike the standalone version.Let us review this simple example to learn a little more about the way that REDUCE works. First, we note that REDUCE deals with variables, and constants like other computer languages, but that in evaluating the former, a variable can stand for itself. Expression evaluation normally follows the rules of high school algebra, so the only surprise in the above example might be that the expression was expanded. REDUCE normally expands expressions where possible, collecting like terms and ordering the variables in a specific manner. However, expansion, ordering of variables, format of output and so on is under control of the user, and various declarations are available to manipulate these.Another characteristic of the above example is the use of lower case on input and upper case on output. In fact, input may be in either mode, but output is usually in lower case. To make the difference between input and output more distinct in this manual, all expressions intended for input will be shown in lower case and output in upper case. However, for stylistic reasons, we represent all single identifiers in the text in upper case.Finally, the numerical prompt can be used to reference the result in a later computation. As a further illustration of the system features, the user should try:reduce> for i:= 1:40 product i;The result in this case is the value of 40!,815915283247897734345611269596115894272000000000You can also get the same result by sayingreduce> factorial 40;Since we want exact results in algebraic calculations, it is essential that integer arithmetic be performed to arbitrary precision, as in the above example. Furthermore, the for statement in the above is illustrative of a whole range of combining forms that REDUCE supports for the convenience of the user.Among the many options in REDUCE is the use of other number systems, such as multiple precision floating point with any specified number of digits — of use if roundoff in, say, the 100th digit is all that can be tolerated.In many cases, it is necessary to use the results of one calculation in succeeding calculations. One way to do this is via an assignment for a variable, such asreduce> u := (x+y+z)^2;If we now use u in later calculations, the value of the right-hand side of the above will be used.The results of a given calculation are also saved in the variable ws (for WorkSpace), so this can be used in the next calculation for further processing.For example, the expressionreduce> df(ws,x);following the previous evaluation will calculate the derivative of (x+y+z)^2 with respect to x. Alternatively,reduce> int(ws,y);would calculate the integral of the same expression with respect to y. REDUCE is also capable of handling symbolic matrices. For example,reduce> matrix m(2,2);declares m to be a two by two matrix, andreduce> m := mat((a,b),(c,d));gives its elements values. Expressions that include m and make algebraic sense may now be evaluated, such as 1/m to give the inverse, 2*m - u*m^2 to give us another matrix and det(m) to give us the determinant of m.REDUCE has a wide range of substitution capabilities. The system knows about elementary functions, but does not automatically invoke many of their well-known properties. For example, products of trigonometrical functions are not converted automatically into multiple angle expressions, but if the user wants this, he can say, for example:reduce> (sin(a+b)+cos(a+b))*(sin(a-b)-cos(a-b))  \n            where cos(~x)*cos(~y) => (cos(x+y)+cos(x-y))/2,  \n                  cos(~x)*sin(~y) => (sin(x+y)-sin(x-y))/2,  \n                  sin(~x)*sin(~y) => (cos(x-y)-cos(x+y))/2;where the tilde in front of the variables x and y indicates that the rules apply for all values of those variables. The result of this calculation is - (cos(2*a) + sin(2*b))See also the user-contributed packages ASSIST (chapter 16.5), CAMAL (chapter 16.10) and TRIGSIMP (chapter 16.72).Another very commonly used capability of the system, and an illustration of one of the many output modes of REDUCE, is the ability to output results in a FORTRAN compatible form. Such results can then be used in a FORTRAN based numerical calculation. This is particularly useful as a way of generating algebraic formulas to be used as the basis of extensive numerical calculations.For example, the statementsreduce> on fort;  \nreduce> df(log(x)*(sin(x)+cos(x))/sqrt(x),x,2);will result in the output      ans=(-(4.0*cos(x)*log(x)*x**2+4.0*cos(x)*log(x)*x-3.0*cos(x)*\n     . log(x)-8.0*cos(x)*x+8.0*cos(x)+4.0*log(x)*sin(x)*x**2-4.0*log(\n     . x)*sin(x)*x-3.0*log(x)*sin(x)+8.0*sin(x)*x+8.0*sin(x)))/(4.0*\n     . sqrt(x)*x**2)These algebraic manipulations illustrate the algebraic mode of REDUCE. REDUCE is based on Standard Lisp. A symbolic mode is also available for executing Lisp statements. These statements follow the syntax of Lisp, e.g.reduce> symbolic car ’(a);Communication between the two modes is possible.With this simple introduction, you are now in a position to study the material in the full REDUCE manual in order to learn just how extensive the range of facilities really is. If further tutorial material is desired, the seven REDUCE Interactive Lessons by David R. Stoutemyer are recommended. These are normally distributed with the system."
},

{
    "location": "man/02-structure.html#",
    "page": "2 Structure of Programs",
    "title": "2 Structure of Programs",
    "category": "page",
    "text": ""
},

{
    "location": "man/02-structure.html#Structure-of-Programs-1",
    "page": "2 Structure of Programs",
    "title": "2 Structure of Programs",
    "category": "section",
    "text": "A REDUCE program consists of a set of functional commands which are evaluated sequentially by the computer. These commands are built up from declarations, statements and expressions. Such entities are composed of sequences of numbers, variables, operators, strings, reserved words and delimiters (such as commas and parentheses), which in turn are sequences of basic characters.Pages = [\"02-structure.md\"]"
},

{
    "location": "man/02-structure.html#.1-The-REDUCE-Standard-Character-Set-1",
    "page": "2 Structure of Programs",
    "title": "2.1 The REDUCE Standard Character Set",
    "category": "section",
    "text": "The basic characters which are used to build REDUCE symbols are the following:The 26 letters a through z\nThe 10 decimal digits 0 through 9\nThe special characters _ ! ~ $ % ’ ( ) * + , - . / : ; < > = { }⟨blank⟩With the exception of strings and characters preceded by an exclamation mark, the case of characters is ignored: depending of the underlying LISP they will all be converted internally into lower case or upper case: ALPHA, Alpha and alpha represent the same symbol. Most implementations allow you to switch this conversion off. The operating instructions for a particular implementation should be consulted on this point. For portability, we shall limit ourselves to the standard character set in this exposition."
},

{
    "location": "man/02-structure.html#Reduce.Algebra.scientific_notation",
    "page": "2 Structure of Programs",
    "title": "Reduce.Algebra.scientific_notation",
    "category": "function",
    "text": "Algebra.scientific_notation(::Union{Number,Tuple,Vector})\n\nThe declaration scientific_notation controls the output format of floating point numbers. At the default settings, any number with five or less digits before the decimal point is printed in a fixed-point notation, e.g., 12345.6. Numbers with more than five digits are printed in scientific notation, e.g., 1.234567E+5. Similarly, by default, any number with eleven or more zeros after the decimal point is printed in scientific notation. To change these defaults, scientific_notation can be used in one of two ways.\n\njulia> Algebra.scientific_notation(m);\n\nwhere m is a positive integer, sets the printing format so that a number with more than m digits before the decimal point, or m or more zeros after the decimal point, is printed in scientific notation.\n\njulia> Algebra.scientific_notation(m,n);\n\nwith m and n both positive integers, sets the format so that a number with more than m digits before the decimal point, or n or more zeros after the decimal point is printed in scientific notation.\n\n\n\n"
},

{
    "location": "man/02-structure.html#.2-Numbers-1",
    "page": "2 Structure of Programs",
    "title": "2.2 Numbers",
    "category": "section",
    "text": "There are several different types of numbers available in REDUCE. Integers consist of a signed or unsigned sequence of decimal digits written without a decimal point, for example:-2, 5396, +32In principle, there is no practical limit on the number of digits permitted as exact arithmetic is used in most implementations. (You should however check the specific instructions for your particular system implementation to make sure that this is true.) For example, if you ask for the value of 2^2000 you get it displayed as a number of 603 decimal digits, taking up several lines of output on an interactive display. It should be borne in mind of course that computations with such long numbers can be quite slow.Numbers that aren’t integers are usually represented as the quotient of two integers, in lowest terms: that is, as rational numbers.In essentially all versions of REDUCE it is also possible (but not always desirable!) to ask REDUCE to work with floating point approximations to numbers again, to any precision. Such numbers are called real. They can be input in two ways:as a signed or unsigned sequence of any number of decimal digits with an embedded or trailing decimal point.\nas in 1. followed by a decimal exponent which is written as the letter E followed by a signed or unsigned integer.e.g. 32. +32.0 0.32E2 and 320.E-1 are all representations of 32.Reduce.Algebra.scientific_notationCAUTION: The unsigned part of any number may not begin with a decimal point, as this causes confusion with the CONS (.) operator, i.e., NOT ALLOWED ARE: .5 -.23 +.12; use 0.5 -0.23 +0.12 instead."
},

{
    "location": "man/02-structure.html#.3-Identifiers-1",
    "page": "2 Structure of Programs",
    "title": "2.3 Identifiers",
    "category": "section",
    "text": "Identifiers in REDUCE consist of one or more alphanumeric characters (i.e. alphabetic letters or decimal digits) the first of which must be alphabetic. The maximum number of characters allowed is implementation dependent, although twenty-four is permitted in most implementations. In addition, the underscore character _ is considered a letter if it is within an identifier. For example,a az p1 q23p  a_very_long_variableare all identifiers, whereas_ais not.A sequence of alphanumeric characters in which the first is a digit is interpreted as a product. For example, 2ab3c is interpreted as 2*ab3c. There is one exception to this: If the first letter after a digit is E, the system will try to interpret that part of the sequence as a real number, which may fail in some cases. For example, 2E12 is the real number 2.0 * 1012, 2e3c is 2000.0*C, and 2ebc gives an error.Special characters, such as -, *, and <blank>, may be used in identifiers too, even as the first character, but each must be preceded by an exclamation mark in input. For example:light!-years    d!\\*!\\*n         good! morning  \n!$sign          !5goldringsCAUTION: Many system identifiers have such special characters in their names (especially * and =). If the user accidentally picks the name of one of them for his own purposes it may have catastrophic consequences for his REDUCE run. Users are therefore advised to avoid such names.Identifiers are used as variables, labels and to name arrays, operators and procedures. RestrictionsThe reserved words listed in section Appendix A: Reserved Identifiers may not be used as identifiers. No spaces may appear within an identifier, and an identifier may not extend over a line of text."
},

{
    "location": "man/02-structure.html#.4-Variables-1",
    "page": "2 Structure of Programs",
    "title": "2.4 Variables",
    "category": "section",
    "text": "Every variable is named by an identifier, and is given a specific type. The type is of no concern to the ordinary user. Most variables are allowed to have the default type, called scalar. These can receive, as values, the representation of any ordinary algebraic expression. In the absence of such a value, they stand for themselves."
},

{
    "location": "man/02-structure.html#Reserved-Variables-1",
    "page": "2 Structure of Programs",
    "title": "Reserved Variables",
    "category": "section",
    "text": "Several variables in REDUCE have particular properties which should not be changed by the user. These variables include:"
},

{
    "location": "man/02-structure.html#CATALAN-1",
    "page": "2 Structure of Programs",
    "title": "CATALAN",
    "category": "section",
    "text": "Catalan\'s constant, defined assum_n=0^infty frac(-1)^n(2n+1)^2"
},

{
    "location": "man/02-structure.html#E-1",
    "page": "2 Structure of Programs",
    "title": "E",
    "category": "section",
    "text": "Intended to represent the base of the natural logarithms. log(e), if it occurs in an expression, is automatically replaced by 1. If rounded is on, E is replaced by the value of e to the current degree of floating point precision."
},

{
    "location": "man/02-structure.html#Reduce.Algebra.euler_gamma",
    "page": "2 Structure of Programs",
    "title": "Reduce.Algebra.euler_gamma",
    "category": "function",
    "text": "euler_gamma()\n\nEuler\'s constant, also available as -psi(1).\n\n\n\n"
},

{
    "location": "man/02-structure.html#EULER_GAMMA-1",
    "page": "2 Structure of Programs",
    "title": "EULER_GAMMA",
    "category": "section",
    "text": "Reduce.Algebra.euler_gamma"
},

{
    "location": "man/02-structure.html#GOLDEN_RATIO-1",
    "page": "2 Structure of Programs",
    "title": "GOLDEN_RATIO",
    "category": "section",
    "text": "The number frac1+sqrt52."
},

{
    "location": "man/02-structure.html#I-1",
    "page": "2 Structure of Programs",
    "title": "I",
    "category": "section",
    "text": "Intended to represent the square root of -1. i^2 is replaced by -1, and appropriately for higher powers of I. This applies only to the symbol I used on the top level, not as a formal parameter in a procedure, a local variable, nor in the context for i:= ...."
},

{
    "location": "man/02-structure.html#INFINITY-1",
    "page": "2 Structure of Programs",
    "title": "INFINITY",
    "category": "section",
    "text": "Intended to represent ∞ in limit and power series calculations for example, as well as in definite integration. Note however that the current system does not do proper arithmetic on ∞. For example, infinity + infinity is 2*infinity."
},

{
    "location": "man/02-structure.html#Reduce.Algebra.khinchin",
    "page": "2 Structure of Programs",
    "title": "Reduce.Algebra.khinchin",
    "category": "function",
    "text": "khinchin()\n\nKhinchin\'s constant, defined as\n\n$ \\prod_{n=1}^\\infty \\left( 1 + \\frac{1}{n(n+2)} \\right)^{\\log_2 n}. $\n\n\n\n"
},

{
    "location": "man/02-structure.html#KHINCHIN-1",
    "page": "2 Structure of Programs",
    "title": "KHINCHIN",
    "category": "section",
    "text": "Reduce.Algebra.khinchin"
},

{
    "location": "man/02-structure.html#NEGATIVE-1",
    "page": "2 Structure of Programs",
    "title": "NEGATIVE",
    "category": "section",
    "text": "Used in the Roots package."
},

{
    "location": "man/02-structure.html#NIL-1",
    "page": "2 Structure of Programs",
    "title": "NIL",
    "category": "section",
    "text": "In REDUCE (algebraic mode only) taken as a synonym for zero. Therefore nil cannot be used as a variable."
},

{
    "location": "man/02-structure.html#PI-1",
    "page": "2 Structure of Programs",
    "title": "PI",
    "category": "section",
    "text": "Intended to represent the circular constant. With rounded on, it is replaced by the value of π to the current degree of floating point precision."
},

{
    "location": "man/02-structure.html#POSITIVE-1",
    "page": "2 Structure of Programs",
    "title": "POSITIVE",
    "category": "section",
    "text": "Used in the Roots package."
},

{
    "location": "man/02-structure.html#T-1",
    "page": "2 Structure of Programs",
    "title": "T",
    "category": "section",
    "text": "Must not be used as a formal parameter or local variable in procedures, since conflict arises with the symbolic mode meaning of T as true.Other reserved variables, such as low_pow, described in other sections, are listed in Appendix A: Reserved Identifiers.Using these reserved variables inappropriately will lead to errors.There are also internal variables used by REDUCE that have similar restrictions. These usually have an asterisk in their names, so it is unlikely a casual user would use one. An example of such a variable is K!\\* used in the asymptotic command package.Certain words are reserved in REDUCE. They may only be used in the manner intended. A list of these is given in the section “Reserved Identifiers”. There are, of course, an impossibly large number of such names to keep in mind. The reader may therefore want to make himself a copy of the list, deleting the names he doesn’t think he is likely to use by mistake."
},

{
    "location": "man/02-structure.html#.5-Strings-1",
    "page": "2 Structure of Programs",
    "title": "2.5 Strings",
    "category": "section",
    "text": "Strings are used in write statements, in other output statements (such as error messages), and to name files. A string consists of any number of characters enclosed in double quotes. For example:~A String~Lower case characters within a string are not converted to upper case.The string ~~ represents the empty string. A double quote may be included in a string by preceding it by another double quote. Thus ~a~~b~ is the string a~b, and ~~~~ is the string consisting of the single character ~.Note that the Reduce.jl parser does not currently support REDUCE strings, as there is no need for them due to the native string support of the Julia language."
},

{
    "location": "man/02-structure.html#.6-Comments-1",
    "page": "2 Structure of Programs",
    "title": "2.6 Comments",
    "category": "section",
    "text": "Text can be included in program listings for the convenience of human readers, in such a way that REDUCE pays no attention to it. There are two ways to do this:Everything from the word comment to the next statement terminator, normally ; or $, is ignored. Such comments can be placed anywhere a blank could properly appear. (Note that end and >> are not treated as comment delimiters!)\nEverything from the symbol % to the end of the line on which it appears is ignored. Such comments can be placed as the last part of any line. Statement terminators have no special meaning in such comments. Remember to put a semicolon before the % if the earlier part of the line is intended to be so terminated. Remember also to begin each line of a multi-line % comment with a % sign."
},

{
    "location": "man/02-structure.html#.7-Operators-1",
    "page": "2 Structure of Programs",
    "title": "2.7 Operators",
    "category": "section",
    "text": "Operators in REDUCE are specified by name and type. There are two types, infix and prefix. Operators can be purely abstract, just symbols with no properties; they can have values assigned (using := or simple let declarations) for specific arguments; they can have properties declared for some collection of arguments (using more general let declarations); or they can be fully defined (usually by a procedure declaration).Infix operators have a definite precedence with respect to one another, and normally occur between their arguments. For example:a + b - c   (spaces optional)\nx<y and y=z (spaces required where shown)Spaces can be freely inserted between operators and variables or operators and operators. They are required only where operator names are spelled out with letters (such as the and in the example) and must be unambiguously separated from another such or from a variable (like Y). Wherever one space can be used, so can any larger number.Prefix operators occur to the left of their arguments, which are written as a list enclosed in parentheses and separated by commas, as with normal mathematical functions, e.g.,cos(u)  \ndf(x^2,x)  \nq(v+w)Unmatched parentheses, incorrect groupings of infix operators and the like, naturally lead to syntax errors. The parentheses can be omitted (replaced by a space following the operator name) if the operator is unary and the argument is a single symbol or begins with a prefix operator name:cos y         means cos(y)\ncos (-y)      – parentheses necessary\nlog cos y     means log(cos(y))\nlog cos (a+b) means log(cos(a+b))butcos a*b       means (cos a)*b\ncos -y        is erroneous (treated as a variable\n              “cos” minus the variable y)A unary prefix operator has a precedence higher than any infix operator, including unary infix operators. In other words, REDUCE will always interpret cos y + 3 as (cos y) + 3 rather than as cos(y + 3).Infix operators may also be used in a prefix format on input, e.g., +(a,b,c). On output, however, such expressions will always be printed in infix form (i.e., a + b + c for this example).A number of prefix operators are built into the system with predefined properties. Users may also add new operators and define their rules for simplification. The built in operators are described in another section."
},

{
    "location": "man/02-structure.html#Built-In-Infix-Operators-1",
    "page": "2 Structure of Programs",
    "title": "Built-In Infix Operators",
    "category": "section",
    "text": "The following infix operators are built into the system. They are all defined internally as procedures.⟨infix operator⟩        where∣:=∣or∣and∣member∣memq∣\n                        =∣neq∣eq∣>=∣>∣<=∣<∣\n                        +∣-∣*∣/∣^∣**∣.These operators may be further divided into the following subclasses:⟨assignment operator⟩   :=\n⟨logical operator⟩      or∣and∣member∣memq\n⟨relational operator⟩   =∣neq∣eq∣>=∣>∣<=∣<\n⟨substitution operator⟩ where\n⟨arithmetic operator⟩   +∣-∣*∣/∣^∣\\*\\*\n⟨construction operator⟩ .memq and eq are not used in the algebraic mode of REDUCE. They are explained in the section on symbolic mode. where is described in the section on substitutions.In previous versions of REDUCE, not was also defined as an infix operator. In the present version it is a regular prefix operator, and interchangeable with null.For compatibility with the intermediate language used by REDUCE, each special character infix operator has an alternative alphanumeric identifier associated with it. These identifiers may be used interchangeably with the corresponding special character names on input. This correspondence is as follows::=   	setq    	(the assignment operator)\n=   	equal    \n>=   	geq    \n>   	greaterp    \n<=   	leq    \n<   	lessp    \n+   	plus    \n-   	difference   	(if unary, minus)\n*   	times    \n/   	quotient    	(if unary, recip)\n^ or ** expt    	(raising to a power)\n.   	cons    Note: neq is used to mean not equal. There is no special symbol provided for it.The above operators are binary, except not which is unary and + and * which are nary (i.e., taking an arbitrary number of arguments). In addition, - and / may be used as unary operators, e.g., /2 means the same as 1/2. Any other operator is parsed as a binary operator using a left association rule. Thus a/b/c is interpreted as (a/b)/c. There are two exceptions to this rule: := and . are right associative. Example: a:=b:=c is interpreted as a:=(b:=c). Unlike ALGOL and PASCAL, ^ is left associative. In other words, a^b^c is interpreted as (a^b)^c.The operators <, <=, >, >= can only be used for making comparisons between numbers. No meaning is currently assigned to this kind of comparison between general expressions.Parentheses may be used to specify the order of combination. If parentheses are omitted then this order is by the ordering of the precedence list defined by the right-hand side of the ⟨infix operator⟩ table at the beginning of this section, from lowest to highest. In other words, where has the lowest precedence, and . (the dot operator) the highest."
},

{
    "location": "man/03-expressions.html#",
    "page": "3 Expressions",
    "title": "3 Expressions",
    "category": "page",
    "text": ""
},

{
    "location": "man/03-expressions.html#Expressions-1",
    "page": "3 Expressions",
    "title": "3 Expressions",
    "category": "section",
    "text": "REDUCE expressions may be of several types and consist of sequences of numbers, variables, operators, left and right parentheses and commas. The most common types are as follows:Pages = [\"03-expressions.md\"]Reduce expressions encapsulated into RExpr objects can be manipulated within julia using the standard syntax. Create an expression object either using the RExpr(\"expression\") string constructor or R\"expression\". Additionally, arbitrary julia expressions can also be parsed directly using the RExpr(expr) constructor. Internally RExpr objects are represented as an array that can be accessed by calling *.str[n] on the object. Julia abstract syntax trees are automatically converted into sequences of reduce statements (using RExpr constructor)."
},

{
    "location": "man/03-expressions.html#.1-Scalar-Expressions-1",
    "page": "3 Expressions",
    "title": "3.1 Scalar Expressions",
    "category": "section",
    "text": "Using the arithmetic operations +, -, *, /, ^ (power) and parentheses, scalar expressions are composed from numbers, ordinary “scalar” variables (identifiers), array names with subscripts, operator or procedure names with arguments and statement expressions.Examples:RExpr(\"x\")\nR\"x^3 - 2*y/(2*z^2 - df(x,z))\"\nR\"(p^2 + m^2)^(1/2)*log (y/m)\"\nR\"a(5) + b(i,q)\"The symbol ** may be used as an alternative to the caret symbol (^) for forming powers, particularly in those systems that do not support a caret symbol.Statement expressions, usually in parentheses, can also form part of a scalar expression, as in the exampleR\"w + (c:=x+y) + z\"When the algebraic value of an expression is needed, REDUCE determines it, starting with the algebraic values of the parts, roughly as follows:Variables and operator symbols with an argument list have the algebraic values they were last assigned, or if never assigned stand for themselves. However, array elements have the algebraic values they were last assigned, or, if never assigned, are taken to be 0.Procedures are evaluated with the values of their actual parameters.In evaluating expressions, the standard rules of algebra are applied. Unfortunately, this algebraic evaluation of an expression is not as unambiguous as is numerical evaluation. This process is generally referred to as “simplification” in the sense that the evaluation usually but not always produces a simplified form for the expression.There are many options available to the user for carrying out such simplification. If the user doesn’t specify any method, the default method is used. The default evaluation of an expression involves expansion of the expression and collection of like terms, ordering of the terms, evaluation of derivatives and other functions and substitution for any expressions which have values assigned or declared (see assignments and let statements). In many cases, this is all that the user needs.The declarations by which the user can exercise some control over the way in which the evaluation is performed are explained in other sections. For example, if a real (floating point) number is encountered during evaluation, the system will normally convert it into a ratio of two integers. If the user wants to use real arithmetic, he can effect this by the command rounded(true). Other modes for coefficient arithmetic are described elsewhere.If an illegal action occurs during evaluation (such as division by zero) or functions are called with the wrong number of arguments, and so on, an appropriate error message is generated."
},

{
    "location": "man/03-expressions.html#.2-Integer-Expressions-1",
    "page": "3 Expressions",
    "title": "3.2 Integer Expressions",
    "category": "section",
    "text": "These are expressions which, because of the values of the constants and variables in them, evaluate to whole numbers.Examples:R\"2\";      R\"37 * 999\";       R\"(x + 3)^2 - x^2 - 6*x\"are obviously integer expressions.R\"j + k - 2 * j^2\"is an integer expression when J and K have values that are integers, or if not integers are such that “the variables and fractions cancel out”, as inR\"k - 7/3 - j + 2/3 + 2*j^2\""
},

{
    "location": "man/03-expressions.html#.3-Boolean-Expressions-1",
    "page": "3 Expressions",
    "title": "3.3 Boolean Expressions",
    "category": "section",
    "text": "Not initially supported by Reduce.jl parser, see upstream docs for more information."
},

{
    "location": "man/03-expressions.html#Reduce.Algebra.lhs",
    "page": "3 Expressions",
    "title": "Reduce.Algebra.lhs",
    "category": "function",
    "text": "lhs(::Union{Expr,RExpr})\n\nReturns the left-hand side of an equation.\n\nExamples\n\njulia> Algebra.lhs(R\"a+b=c\")\n\na + b\n\n\n\n\n"
},

{
    "location": "man/03-expressions.html#Reduce.Algebra.rhs",
    "page": "3 Expressions",
    "title": "Reduce.Algebra.rhs",
    "category": "function",
    "text": "rhs(::Union{Expr,RExpr})\n\nReturns the right-hand side of an equation.\n\nExamples\n\njulia> Algebra.rhs(R\"a+b=c\")\n\nc\n\n\n\n\n"
},

{
    "location": "man/03-expressions.html#.4-Equations-1",
    "page": "3 Expressions",
    "title": "3.4 Equations",
    "category": "section",
    "text": "Equations are a particular type of expression with the syntaxR\"⟨expression⟩=⟨expression⟩\"In addition to their role as boolean expressions, they can also be used as arguments to several operators (e.g., solve), and can be returned as values.Under normal circumstances, the right-hand-side of the equation is evaluated but not the left-hand-side. This also applies to any substitutions made by the sub operator. If both sides are to be evaluated, the switch evallhseqp should be turned on.To facilitate the handling of equations, two selectors, lhs and rhs, which return the left- and right-hand sides of an equation respectively, are provided.Reduce.Algebra.lhsReduce.Algebra.rhs"
},

{
    "location": "man/03-expressions.html#.5-Proper-Statements-as-Expressions-1",
    "page": "3 Expressions",
    "title": "3.5 Proper Statements as Expressions",
    "category": "section",
    "text": "Several kinds of proper statements deliver an algebraic or numerical result of some kind, which can in turn be used as an expression or part of an expression. For example, an assignment statement itself has a value, namely the value assigned. SoR\"2 * (x := a+b)\"is equal to R\"2*(a+b)\", as well as having the “side-effect” of assigning the value a+b to X. In context,R\"y := 2 * (x := a+b);\"sets X to a+b and Y to 2*(a+b).Note that if the Reduce.jl parser is used to convert these types of expressions to Julia AST, issues can occur since an equivalent feature does not exist in the Julia language.The sections on the various proper statement types indicate which of these statements are also useful as expressions."
},

{
    "location": "man/04-lists.html#",
    "page": "4 Lists",
    "title": "4 Lists",
    "category": "page",
    "text": ""
},

{
    "location": "man/04-lists.html#Lists-1",
    "page": "4 Lists",
    "title": "4 Lists",
    "category": "section",
    "text": "A list is an object consisting of a sequence of other objects (including lists themselves), separated by commas and surrounded by braces. Examples of lists are:R\"{a,b,c}\"\nR\"{1,a-b,c=d}\"\nR\"{{a},{{b,c},d},e}\"The empty list is represented asR\"{}\"Pages = [\"04-lists.md\"]"
},

{
    "location": "man/04-lists.html#.1-Operations-on-Lists-1",
    "page": "4 Lists",
    "title": "4.1 Operations on Lists",
    "category": "section",
    "text": "Several operators in the system return their results as lists, and a user can create new lists using braces and commas. Alternatively, one can use the operator list to construct a list. An important class of operations on lists are map and select operations. For details, please refer to the chapters on map, select and the for command. See also the documentation on the ASSIST (chapter 16.5) package.To facilitate the use of lists, a number of operators are also available for manipulating them. R\"part(⟨list⟩,n)\" for example will return the nth element of a list. length will return the length of a list. Several operators are also defined uniquely for lists. For those familiar with them, these operators in fact mirror the operations defined for Lisp lists. These operators are as follows:"
},

{
    "location": "man/04-lists.html#Reduce.list",
    "page": "4 Lists",
    "title": "Reduce.list",
    "category": "function",
    "text": "list(r)\n\nThe operator list is an alternative to the usage of curly brackets. list accepts an arbitrary number of arguments and returns a list of its arguments. This operator is useful in cases where operators have to be passed as arguments. E.g.,\n\nlist(:a,list(list(:b,:c),:d),:e)       ->  R\"{{a},{{b,c},d},e}\"\n\n\n\n"
},

{
    "location": "man/04-lists.html#.1.1-LIST-1",
    "page": "4 Lists",
    "title": "4.1.1 LIST",
    "category": "section",
    "text": "Reduce.list"
},

{
    "location": "man/04-lists.html#.1.2-FIRST-1",
    "page": "4 Lists",
    "title": "4.1.2 FIRST",
    "category": "section",
    "text": "This operator returns the first member of a list. An error occurs if the argument is not a list, or the list is empty."
},

{
    "location": "man/04-lists.html#.1.3-SECOND-1",
    "page": "4 Lists",
    "title": "4.1.3 SECOND",
    "category": "section",
    "text": "second returns the second member of a list. An error occurs if the argument is not a list or has no second element."
},

{
    "location": "man/04-lists.html#.1.4-THIRD-1",
    "page": "4 Lists",
    "title": "4.1.4 THIRD",
    "category": "section",
    "text": "This operator returns the third member of a list. An error occurs if the argument is not a list or has no third element."
},

{
    "location": "man/04-lists.html#.1.5-REST-1",
    "page": "4 Lists",
    "title": "4.1.5 REST",
    "category": "section",
    "text": "rest returns its argument with the first element removed. An error occurs if the argument is not a list, or is empty."
},

{
    "location": "man/04-lists.html#.1.6-.-(Cons)-Operator-1",
    "page": "4 Lists",
    "title": "4.1.6 . (Cons) Operator",
    "category": "section",
    "text": "This operator adds (“conses”) an expression to the front of a list. For example:R\"a . {b,c}\"     ->   R\"{a,b,c}\""
},

{
    "location": "man/04-lists.html#.1.7-APPEND-1",
    "page": "4 Lists",
    "title": "4.1.7 APPEND",
    "category": "section",
    "text": "This operator appends its first argument to its second to form a new list. Examples:R\"append({a,b},{c,d})\"     ->     R\"{a,b,c,d}\"\nR\"append({{a,b}},{c,d})\"   ->     R\"{{a,b},c,d}\""
},

{
    "location": "man/04-lists.html#.1.8-REVERSE-1",
    "page": "4 Lists",
    "title": "4.1.8 REVERSE",
    "category": "section",
    "text": "The operator reverse returns its argument with the elements in the reverse order. It only applies to the top level list, not any lower level lists that may occur. Examples are:R\"reverse({a,b,c})\"        ->     R\"{c,b,a}\"\nR\"reverse({{a,b,c},d})\"    ->     R\"{d,{a,b,c}}\""
},

{
    "location": "man/04-lists.html#.1.9-List-Arguments-of-Other-Operators-1",
    "page": "4 Lists",
    "title": "4.1.9 List Arguments of Other Operators",
    "category": "section",
    "text": "If an operator other than those specifically defined for lists is given a single argument that is a list, then the result of this operation will be a list in which that operator is applied to each element of the list. For example, the result of evaluating R\"log{a,b,c}\" is the expression R\"{LOG(A),LOG(B),LOG(C)}\".There are two ways to inhibit this operator distribution. Firstly, the switch listargs, if on, will globally inhibit such distribution. Secondly, one can inhibit this distribution for a specific operator by the declaration listargp. For example, with the declaration R\"listargp log, log{a,b,c}\" would evaluate to R\"log({a,b,c})\".If an operator has more than one argument, no such distribution occurs."
},

{
    "location": "man/04-lists.html#.1.10-Caveats-and-Examples-1",
    "page": "4 Lists",
    "title": "4.1.10 Caveats and Examples",
    "category": "section",
    "text": "Some of the natural list operations such as member or delete are available only after loading the package ASSIST (chapter 16.5).Please note that a non-list as second argument to cons (a \"dotted pair\" in LISP terms) is not allowed and causes an \"invalid as list\" error.R\"a := 17 . 4\"\n\n***** 17 4 invalid as listAlso, the initialization of a scalar variable is not the empty list – one has to set list type variables explicitly, as in the following example: load_package assist;  \n \n procedure lotto (n,m);  \n  begin scalar list_1_n, luckies, hit;  \n     list_1_n := {};  \n     luckies := {};  \n     for k:=1:n do list_1_n := k . list_1_n;  \n     for k:=1:m do  \n       << hit := part(list_1_n,random(n-k+1) + 1);  \n          list_1_n := delete(hit,list_1_n);  \n          luckies := hit . luckies >>;  \n     return luckies;  \n  end;  \n                 % In Germany, try lotto (49,6);Another example: Find all coefficients of a multivariate polynomial with respect to a list of variables:procedure allcoeffs(q,lis);  \n   % q : polynomial, lis: list of vars  \n   allcoeffs1 (list q,lis);  \n \nprocedure allcoeffs1(q,lis);  \n  if lis={} then q else  \n    allcoeffs1(foreach qq in q join coeff(qq,first lis),  \n               rest lis);"
},

{
    "location": "man/05-statements.html#",
    "page": "5 Statements",
    "title": "5 Statements",
    "category": "page",
    "text": ""
},

{
    "location": "man/05-statements.html#Statements-1",
    "page": "5 Statements",
    "title": "5 Statements",
    "category": "section",
    "text": "A statement is any combination of reserved words and expressions, and has the syntaxR\"⟨statement⟩ ::= ⟨expression⟩∣⟨proper statement⟩\"A REDUCE program consists of a series of commands which are statements followed by a terminator:⟨terminator⟩ ::= ;∣$The division of the program into lines is arbitrary. Several statements can be on one line, or one statement can be freely broken onto several lines. If the program is run interactively, statements ending with ; or $ are not processed until an end-of-line character is encountered. This character can vary from system to system, but is normally the Return key on an ASCII terminal. Specific systems may also use additional keys as statement terminators.If a statement is a proper statement, the appropriate action takes place.Depending on the nature of the proper statement some result or response may or may not be printed out, and the response may or may not depend on the terminator used.If a statement is an expression, it is evaluated. If the terminator is a semicolon, the result is printed. If the terminator is a dollar sign, the result is not printed. Because it is not usually possible to know in advance how large an expression will be, no explicit format statements are offered to the user. However, a variety of output declarations are available so that the output can be produced in different forms. These output declarations are explained in Section 8.3.3.The following sub-sections describe the types of proper statements in REDUCE.Pages = [\"05-statements.md\"]"
},

{
    "location": "man/05-statements.html#.1-Assignment-Statements-1",
    "page": "5 Statements",
    "title": "5.1 Assignment Statements",
    "category": "section",
    "text": "These statements have the syntax⟨assignment statement⟩ ::= ⟨expression⟩:=⟨expression⟩The ⟨expression⟩ on the left side is normally the name of a variable, an operator symbol with its list of arguments filled in, or an array name with the proper number of integer subscript values within the array bounds. For example:R\"a1 := b + c\"\nR\"h(l,m) := x-2*y\"     	(where h is an operator)\nR\"k(3,5) := x-2*y\"		(where k is a 2-dim. array)More general assignments such as R\"a+b := c\" are also allowed. The effect of these is explained in Section 11.2.5.An assignment statement causes the expression on the right-hand-side to be evaluated. If the left-hand-side is a variable, the value of the right-hand-side is assigned to that unevaluated variable. If the left-hand-side is an operator or array expression, the arguments of that operator or array are evaluated, but no other simplification done. The evaluated right-hand-side is then assigned to the resulting expression. For example, if a is a single-dimensional array, R\"a(1+1) := b\" assigns the value b to the array element a(2).If a semicolon is used as the terminator when an assignment is issued as a command (i.e. not as a part of a group statement or procedure or other similar construct), the left-hand side symbol of the assignment statement is printed out, followed by a “:=”, followed by the value of the expression on the right.It is also possible to write a multiple assignment statement:⟨expression⟩:=…:=⟨expression⟩:=⟨expression⟩In this form, each ⟨expression⟩ but the last is set to the value of the last ⟨expression⟩. If a semicolon is used as a terminator, each expression except the last is printed followed by a “:=” ending with the value of the last expression."
},

{
    "location": "man/05-statements.html#Reduce.Algebra.set",
    "page": "5 Statements",
    "title": "Reduce.Algebra.set",
    "category": "function",
    "text": "set(a,b)\n\nIn some cases, it is desirable to perform an assignment in which both the left- and right-hand sides of an assignment are evaluated. In this case, the set statement can be used with the syntax:\n\nR\"set(⟨expression⟩,⟨expression⟩)\"\n\nFor example, the statements\n\n        j := 23;  \n        set(mkid(a,j),x);\n\nassigns the value x to a23.\n\n\n\n"
},

{
    "location": "man/05-statements.html#Reduce.Algebra.unset",
    "page": "5 Statements",
    "title": "Reduce.Algebra.unset",
    "category": "function",
    "text": "unset(r)\n\nTo remove a value from such a variable, the unset statement can be used with the syntax:\n\nR\"unset(⟨expression⟩)\"\n\nFor example, the statement\n\n        j := 23;  \n        unset(mkid(a,j));\n\nclears the value of a23.\n\n\n\n"
},

{
    "location": "man/05-statements.html#.1.1-Set-and-Unset-Statements-1",
    "page": "5 Statements",
    "title": "5.1.1 Set and Unset Statements",
    "category": "section",
    "text": "Reduce.Algebra.setReduce.Algebra.unset"
},

{
    "location": "man/05-statements.html#.2-Group-Statements-1",
    "page": "5 Statements",
    "title": "5.2 Group Statements",
    "category": "section",
    "text": "The group statement is a construct used where REDUCE expects a single statement, but a series of actions needs to be performed. It is formed by enclosing one or more statements (of any kind) between the symbols << and >>, separated by semicolons or dollar signs – it doesn’t matter which. The statements are executed one after another.Examples will be given in the sections on if and other types of statements in which the <<…>> construct is useful.If the last statement in the enclosed group has a value, then that is also the value of the group statement. Care must be taken not to have a semicolon or dollar sign after the last grouped statement, if the value of the group is relevant: such an extra terminator causes the group to have the value nil or zero."
},

{
    "location": "man/05-statements.html#.3-Conditional-Statements-1",
    "page": "5 Statements",
    "title": "5.3 Conditional Statements",
    "category": "section",
    "text": "Not initially supported by Reduce.jl parser, see upstream docs for more information."
},

{
    "location": "man/05-statements.html#.4-FOR-Statements-1",
    "page": "5 Statements",
    "title": "5.4 FOR Statements",
    "category": "section",
    "text": "Not initially supported by Reduce.jl parser, see upstream docs for more information."
},

{
    "location": "man/05-statements.html#.5-WHILE-…DO-1",
    "page": "5 Statements",
    "title": "5.5 WHILE …DO",
    "category": "section",
    "text": "Not initially supported by Reduce.jl parser, see upstream docs for more information."
},

{
    "location": "man/05-statements.html#.6-REPEAT-…UNTIL-1",
    "page": "5 Statements",
    "title": "5.6 REPEAT …UNTIL",
    "category": "section",
    "text": "Not initially supported by Reduce.jl parser, see upstream docs for more information."
},

{
    "location": "man/05-statements.html#.7-Compound-Statements-1",
    "page": "5 Statements",
    "title": "5.7 Compound Statements",
    "category": "section",
    "text": "Not initially supported by Reduce.jl parser, see upstream docs for more information."
},

{
    "location": "man/06-commands-declarations.html#",
    "page": "6 Commands and Declarations",
    "title": "6 Commands and Declarations",
    "category": "page",
    "text": ""
},

{
    "location": "man/06-commands-declarations.html#Commands-and-Declarations-1",
    "page": "6 Commands and Declarations",
    "title": "6 Commands and Declarations",
    "category": "section",
    "text": "A command is an order to the system to do something. Some commands cause visible results (such as calling for input or output); others, usually called declarations, set options, define properties of variables, or define procedures. Commands are formally defined as a statement followed by a terminator⟨command⟩ 	::= ⟨statement⟩⟨terminator⟩\n⟨terminator⟩ 	::= ;∣$Some REDUCE commands and declarations are described in the following sub-sections.Pages = [\"06-commands-declarations.md\"]"
},

{
    "location": "man/06-commands-declarations.html#.1-Array-Declarations-1",
    "page": "6 Commands and Declarations",
    "title": "6.1 Array Declarations",
    "category": "section",
    "text": "Not initially supported by Reduce.jl parser, see upstream docs for more information."
},

{
    "location": "man/06-commands-declarations.html#Reduce.Algebra.on",
    "page": "6 Commands and Declarations",
    "title": "Reduce.Algebra.on",
    "category": "function",
    "text": "on(::Symbol...)\n\nTakes a list of switch names as argument and turns them on.\n\n\n\n"
},

{
    "location": "man/06-commands-declarations.html#Reduce.Algebra.off",
    "page": "6 Commands and Declarations",
    "title": "Reduce.Algebra.off",
    "category": "function",
    "text": "off(::Symbol...)\n\nTakes a list of switch names as argument and turns them off.\n\n\n\n"
},

{
    "location": "man/06-commands-declarations.html#.2-Mode-Handling-Declarations-1",
    "page": "6 Commands and Declarations",
    "title": "6.2 Mode Handling Declarations",
    "category": "section",
    "text": "The on and off declarations are available to the user for controlling various system options. Each option is represented by a switch name. on and off take a list of switch names as argument and turn them on and off respectively, e.g.,julia> on(:time)causes the system to print a message after each command giving the elapsed CPU time since the last command, or since time was last turned off, or the session began. Another useful switch with interactive use is demo, which causes the system to pause after each command in a file (with the exception of comments) until a <Return> is typed on the terminal. This enables a user to set up a demonstration file and step through it command by command.Reduce.Algebra.onReduce.Algebra.offAs with most declarations, arguments to on and off may be strung together separated by commas. For example,julia> off(:time,:demo)will turn off both the time messages and the demonstration switch.We note here that while most on and off commands are obeyed almost instantaneously, some trigger time-consuming actions such as reading in necessary modules from secondary storage.A diagnostic message is printed if on or off are used with a switch that is not known to the system. For example, if you misspell demo and typejulia> on(:demq)you will get the messageERROR: Reduce: \n***** demq not defined as switch "
},

{
    "location": "man/06-commands-declarations.html#.3-END-1",
    "page": "6 Commands and Declarations",
    "title": "6.3 END",
    "category": "section",
    "text": "The identifier end has two separate uses.Its use in a R\"begin… end\" bracket has been discussed in connection with compound statements.\nFiles to be read using IN should end with an extra end; command. The reason for this is explained in the section on the IN command. This use of END does not allow an immediately preceding end (such as the end of a procedure definition), so we advise using ;end; there."
},

{
    "location": "man/06-commands-declarations.html#.4-BYE-Command-1",
    "page": "6 Commands and Declarations",
    "title": "6.4 BYE Command",
    "category": "section",
    "text": "The command R\"bye\" (or alternatively R\"quit\") stops the execution of REDUCE, closes all open output files, and returns you to the calling program (usually the operating system). Your REDUCE session is normally destroyed."
},

{
    "location": "man/06-commands-declarations.html#.5-SHOWTIME-Command-1",
    "page": "6 Commands and Declarations",
    "title": "6.5 SHOWTIME Command",
    "category": "section",
    "text": "R\"showtime\" prints the elapsed time since the last call of this command or, on its first call, since the current REDUCE session began. The time is normally given in milliseconds and gives the time as measured by a system clock. The operations covered by this measure are system dependent."
},

{
    "location": "man/06-commands-declarations.html#.6-DEFINE-Command-1",
    "page": "6 Commands and Declarations",
    "title": "6.6 DEFINE Command",
    "category": "section",
    "text": "Not initially supported by Reduce.jl parser, see upstream docs for more information."
},

{
    "location": "man/07-prefix-ops.html#",
    "page": "7 Built-in Prefix Operators",
    "title": "7 Built-in Prefix Operators",
    "category": "page",
    "text": ""
},

{
    "location": "man/07-prefix-ops.html#Built-in-Prefix-Operators-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7 Built-in Prefix Operators",
    "category": "section",
    "text": "In the following subsections are descriptions of the most useful prefix operators built into REDUCE that are not defined in other sections (such as substitution operators). Some are fully defined internally as procedures; others are more nearly abstract operators, with only some of their properties known to the system.In many cases, an operator is described by a prototypical header line as follows. Each formal parameter is given a name and followed by its allowed type. The names of classes referred to in the definition are printed in lower case, and parameter names in upper case. If a parameter type is not commonly used, it may be a specific set enclosed in brackets { … }. Operators that accept formal parameter lists of arbitrary length have the parameter and type class enclosed in square brackets indicating that zero or more occurrences of that argument are permitted. Optional parameters and their type classes are enclosed in angle brackets.Pages = [\"07-prefix-ops.md\"]"
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.abs",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.abs",
    "category": "function",
    "text": "abs(r)\n\nabs returns the absolute value of its single argument, if that argument has a numerical value. A non-numerical argument is returned as an absolute value, with an overall numerical coefficient taken outside the absolute value operator. For example:\n\njulia> Algebra.abs(-3/4)\n0.75\n\njulia> Algebra.abs(:(2a))\n:(2 * abs(a))\n\njulia> Algebra.abs(im)\n1.0\n\njulia> Algebra.abs(:(-x))\n:(abs(x))\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.ceiling",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.ceiling",
    "category": "function",
    "text": "ceiling(r)\n\nThis operator returns the ceiling (i.e., the least integer greater than the given argument) if its single argument has a numerical value. A non-numerical argument is returned as an expression in the original operator. For example:\n\njulia> Algebra.ceiling(-5/4)\n-1\n\njulia> Algebra.ceiling(:(-a))\n:(ceiling(-a))\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.conj",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.conj",
    "category": "function",
    "text": "conj(r)\n\nThis operator returns the ceiling (i.e., the least integer greater than the given argument) if its single argument has a numerical value. A non-numerical argument is returned as an expression in the original operator. For example:\n\njulia> Algebra.conj(1+im)\n1 - 1im\n\njulia> Algebra.conj(:(a+im*b))\n:(repart(a) - ((impart(a) + repart(b)) * im + impart(b)))\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.factorial",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.factorial",
    "category": "function",
    "text": "factorial(r)\n\nIf the single argument of factorial evaluates to a non-negative integer, its factorial is returned. Otherwise an expression involving factorial is returned. For example:\n\njulia> Algebra.factorial(5)\n120\n\njulia> Algebra.factorial(:a)\n:(factorial(a))\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.fix",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.fix",
    "category": "function",
    "text": "fix(r)\n\nThis operator returns the fixed value (i.e., the integer part of the given argument) if its single argument has a numerical value. A non-numerical argument is returned as an expression in the original operator. For example:\n\njulia> Algebra.fix(-5/4)\n-1\n\njulia> Algebra.fix(:a)\n:(fix(a))\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.floor",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.floor",
    "category": "function",
    "text": "floor(r)\n\nThis operator returns the floor (i.e., the greatest integer less than the given argument) if its single argument has a numerical value. A non-numerical argument is returned as an expression in the original operator. For example:\n\njulia> Algebra.floor(-5/4)\n-2.0\n\njulia> Algebra.floor(:a)\n:(floor(a))\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.impart",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.impart",
    "category": "function",
    "text": "impart(r)\n\nThis operator returns the imaginary part of an expression, if that argument has an numerical value. A non-numerical argument is returned as an expression in the operators repart and impart. For example:\n\njulia> Algebra.impart(1+im)\n1\n\njulia> Algebra.impart(:(a+im*b))\n:(impart(a) + repart(b))\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.max",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.max",
    "category": "function",
    "text": "max(r...)\n\nmax can take an arbitrary number of expressions as their arguments. If all arguments evaluate to numerical values, the maximum of the argument list is returned. If any argument is non-numeric, an appropriately reduced expression is returned. For example:\n\njulia> Algebra.max(2,-3,4,5)\n5\n\njulia> Algebra.max(:a,2,3)\n:(max(3, a))\n\nmax of an empty list returns 0.\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.min",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.min",
    "category": "function",
    "text": "min(r...)\n\nmin can take an arbitrary number of expressions as their arguments. If all arguments evaluate to numerical values, the minimum of the argument list is returned. If any argument is non-numeric, an appropriately reduced expression is returned. For example:\n\njulia> Algebra.min(2,-2)\n-2\n\njulia> Algebra.min(:x)\n:x\n\nmin of an empty list returns 0.\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.nextprime",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.nextprime",
    "category": "function",
    "text": "nextprime(r)\n\nnextprime returns the next prime greater than its integer argument, using a probabilistic algorithm. A type error occurs if the value of the argument is not an integer. For example:\n\njulia> Algebra.nextprime(5)\n7\n\njulia> Algebra.nextprime(-2)\n2\n\njulia> Algebra.nextprime(-7)\n-5\n\njulia> Algebra.nextprime(1000000)\n1000003\n\nwhereas Algebra.nextprime(:a) gives a type error.\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.random",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.random",
    "category": "function",
    "text": "random(r)\n\nrandom(n) returns a random number r in the range 0  r  n. A type error occurs if the value of the argument is not a positive integer in algebraic mode, or positive number in symbolic mode. For example:\n\njulia> Algebra.random(5)\n3\n\njulia> Algebra.random(1000)\n191\n\nwhereas Algebra.random(:a) gives a type error.\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.random_new_seed",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.random_new_seed",
    "category": "function",
    "text": "random_new_seed(r)\n\nrandom_new_seed(n) reseeds the random number generator to a sequence determined by the integer argument n. It can be used to ensure that a repeatable pseudo-random sequence will be delivered regardless of any previous use of random, or can be called early in a run with an argument derived from something variable (such as the time of day) to arrange that different runs of a REDUCE program will use different random sequences. When a fresh copy of REDUCE is first created it is as if random_new_seed(1) has been obeyed.\n\nA type error occurs if the value of the argument is not a positive integer.\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.repart",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.repart",
    "category": "function",
    "text": "repart(r)\n\nThis returns the real part of an expression, if that argument has an numerical value. A non-numerical argument is returned as an expression in the operators repart and impart. For example:\n\njulia> Algebra.repart(1+im)\n1\n\njulia> Algebra.repart(:(a+im*b))\n:(repart(a) - impart(b))\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.round",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.round",
    "category": "function",
    "text": "round(r)\n\nThis operator returns the rounded value (i.e, the nearest integer) of its single argument if that argument has a numerical value. A non-numeric argument is returned as an expression in the original operator. For example:\n\njulia> Algebra.round(-5/4)\n-1.0\n\njulia> Algebra.round(:a)\n:(round(a))\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.sign",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.sign",
    "category": "function",
    "text": "sign(r)\n\nsign tries to evaluate the sign of its argument. If this is possible sign returns one of 1, 0 or -1. Otherwise, the result is the original form or a simplified variant. For example:\n\njulia> Algebra.sign(-5)\n-1\n\njulia> Algebra.sign(:(-a^2*b))\n:(-(sign(b)))\n\nNote that even powers of formal expressions are assumed to be positive only as long as the switch complex is off.\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#.1-Numerical-Operators-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.1 Numerical Operators",
    "category": "section",
    "text": "REDUCE includes a number of functions that are analogs of those found in most numerical systems. With numerical arguments, such functions return the expected result. However, they may also be called with non-numerical arguments. In such cases, except where noted, the system attempts to simplify the expression as far as it can. In such cases, a residual expression involving the original operator usually remains. These operators are as follows:Reduce.Algebra.absReduce.Algebra.ceilingReduce.Algebra.conjReduce.Algebra.factorialReduce.Algebra.fixReduce.Algebra.floorReduce.Algebra.impartReduce.Algebra.maxReduce.Algebra.minReduce.Algebra.nextprimeReduce.Algebra.randomReduce.Algebra.random_new_seedReduce.Algebra.repartReduce.Algebra.roundReduce.Algebra.sign"
},

{
    "location": "man/07-prefix-ops.html#.2-Mathematical-Functions-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.2 Mathematical Functions",
    "category": "section",
    "text": "REDUCE knows that the following represent mathematical functions that can take arbitrary scalar expressions as their argument(s):acos acosh acot acoth acsc acsch asec asech asin  \nasinh atan atanh atan2 beta ci cos cosh cot coth csc  \ncsch dilog ei exp gamma hypot ibeta igamma ln log  \nlogb log10 sec sech si sin sinh sqrt tan tanh  \nairy_ai airy_aiprime airy_bi airy_biprime  \nbesseli besselj besselk bessely  \nhankel1 hankel2 kummerm kummeru lommel1 lommel2  \nstruveh struvel whittakerm whittakeru  \npolygamma psi zeta  \nsolidharmonicy sphericalharmonicywhere log is the natural logarithm (and equivalent to ln), and logb has two arguments of which the second is the logarithmic base.The derivatives of all these functions are also known to the system.REDUCE knows various elementary identities and properties of these functions. For example: cos(-x) = cos(x)              sin(-x) = - sin (x)  \n cos(n*pi) = (-1)^n            sin(n*pi) = 0  \n log(e)  = 1                   e^(i*pi/2) = i  \n log(1)  = 0                   e^(i*pi) = -1  \n log(e^x) = x                  e^(3*i*pi/2) = -iBeside these identities, there are a lot of simplifications for elementary functions defined in the REDUCE system as rulelists. In order to view these, the showrules operator can be used, e.g.reduce> showrules tan;  \n \n{tan(~n*arbint(~i)*pi + ~(~ x)) => tan(x) when fixp(n),  \n \n tan(~x)  \n \n  => trigquot(sin(x),cos(x)) when knowledge_about(sin,x,tan),  \n \n      ~x + ~(~ k)*pi  \n tan(----------------)  \n            ~d  \n \n             x                  k                     k     1  \n  =>  - cot(---) + i*pi*impart(---)) when abs(repart(---))=---,  \n             d                  d                     d     2  \n \n      ~(~ w) + ~(~ k)*pi           w      k                k  \n tan(--------------------) => tan(--- + (--- - fix(repart(---)))*pi)  \n            ~(~ d)                 d      d                d  \n \n                              k  \n when whereexp({rp => repart(---)},bool-eval(ratnump(rp) and abs(rp)>=1)),  \n                              d  \n \n tan(atan(~x)) => x,  \n \n                             2  \n df(tan(~x),~x) => 1 + tan(x) }  \nFor further simplification, especially of expressions involving trigonometric functions, see the TRIGSIMP package (chapter 16.72) documentation.Functions not listed above may be defined in the special functions package SPECFN.The user can add further rules for the reduction of expressions involving these operators by using the let command.In many cases it is desirable to expand product arguments of logarithms, or collect a sum of logarithms into a single logarithm. Since these are inverse operations, it is not possible to provide rules for doing both at the same time and preserve the REDUCE concept of idempotent evaluation. As an alternative, REDUCE provides two switches expandlogs and combinelogs to carry out these operations. Both are off by default, and are subject to the value of the switch precise. This switch is on by default and prevents modifications that may be false in a complex domain. Thus to expand log(3*y) into a sum of logs, one can sayjulia> Algebra.on(:expandlogs);\n\njulia> Algebra.log(:(3*y))whereas to expand log(x*y) into a sum of logs, one needs to sayjulia> Algebra.off(:precise); Algebra.on(:expandlogs);\n\njulia> Algebra.log(:(x*y))To combine this sum into a single log:julia> Algebra.off(:precise); Algebra.on(:combinelogs);\n\njulia> Alebra.:+(log(:x),log(:y))These switches affect the logarithmic functions log10 (base 10) and logb (arbitrary base) as well.At the present time, it is possible to have both switches on at once, which could lead to infinite recursion. However, an expression is switched from one form to the other in this case. Users should not rely on this behavior, since it may change in the next release.The current version of REDUCE does a poor job of simplifying surds. In particular, expressions involving the product of variables raised to non-integer powers do not usually have their powers combined internally, even though they are printed as if those powers were combined. For example, the expressionreduce> x^(1/3)*x^(1/6)will print assqrt(x)but will have an internal form containing the two exponentiated terms. If you now subtract sqrt(x) from this expression, you will not get zero. Instead, the confusing formsqrt(x) - sqrt(x)will result. To combine such exponentiated terms, the switch combineexpt should be turned on.The square root function can be input using the name sqrt, or the power operation ^(1/2). On output, unsimplified square roots are normally represented by the operator sqrt rather than a fractional power. With the default system switch settings, the argument of a square root is first simplified, and any divisors of the expression that are perfect squares taken outside the square root argument. The remaining expression is left under the square root. Thus the expressionjulia> Algebra.sqrt(:(-8a^2*b))becomes:(2 * sqrt(b) * sqrt(2) * a * im)Note that such simplifications can cause trouble if A is eventually given a value that is a negative number. If it is important that the positive property of the square root and higher even roots always be preserved, the switch PRECISE should be set on (the default value). This causes any non-numerical factors taken out of surds to be represented by their absolute value form. With PRECISE on then, the above example would become:(2 * sqrt(-2b) * abs(a))However, this is incorrect in the complex domain, where the sqrtx^2 is not identical to x. To avoid the above simplification, the switch precise_complex should be set on (default is off). For example:julia> Algebra.on(:precise_complex); Algebra.sqrt(:(-8a^2*b))yields the output:(2 * sqrt(-2 * a ^ 2 * b))The statement that REDUCE knows very little about these functions applies only in the mathematically exact off rounded mode. If rounded is on, any of the functionsacos acosh acot acoth acsc acsch asec asech asin  \nasinh atan atanh atan2 cos cosh cot coth csc csch  \nexp hypot ibeta igamma ln log logb log10 psi sec  \nsech sin sinh sqrt tan tanhwhen given a numerical argument has its value calculated to the current degree of floating point precision. In addition, real (non-integer valued) powers of numbers will also be evaluated.If the complex switch is turned on in addition to rounded, these functions will also calculate a real or complex result, again to the current degree of floating point precision, if given complex arguments. For example,julia> @rounded @complex 2.3^(5.6im)\n:(-0.0480793490914 - 0.998843519372im)\n\njulia> @rounded @complex cos(2+3im)\n:(-4.18962569097 - 9.10922789376im)"
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.bernoulli",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.bernoulli",
    "category": "function",
    "text": "bernoulli(n)\n\nThe unary operator bernoulli provides notation and computation for Bernoulli numbers. bernoulli(n) evaluates to the nth Bernoulli number; all of the odd Bernoulli numbers, except bernoulli(1), are zero.\n\nThe algorithms are based upon those by Herbert Wilf, presented by Sandra Fillebrown [?]. If the rounded switch is off, the algorithms are exactly those; if it is on, some further rounding may be done to prevent computation of redundant digits. Hence, these functions are particularly fast when used to approximate the Bernoulli numbers in rounded mode.\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#.3-Bernoulli-Numbers-and-Euler-Numbers-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.3 Bernoulli Numbers and Euler Numbers",
    "category": "section",
    "text": "Reduce.Algebra.bernoulliEuler numbers are computed by the unary operator Euler, which return the nth Euler number. The computation is derived directly from Pascal’s triangle of binomial coefficients."
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.fibonacci",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.fibonacci",
    "category": "function",
    "text": "fibonacci(n)\n\nThe unary operator fibonacci provides notation and computation for Fibonacci numbers. fibonacci(n) evaluates to the nth Fibonacci number. If n is a positive or negative integer, it will be evaluated following the definition:\n\nF_0 = 0 F_1 = 1 F_n = F_n-1 + F_n-2\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.fibonaccip",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.fibonaccip",
    "category": "function",
    "text": "fibonacci(n,x)\n\nFibonacci Polynomials are computed by the binary operator fibonaccip. fibonaccip(n,x) returns the nth Fibonacci polynomial in the variable x. If n is a positive or negative integer, it will be evaluated following the definition:\n\nF_0(x) = 0 F_1(x) = 1 F_n(x) = xF_n-1(x) + F_n-2(x)\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#.4-Fibonacci-Numbers-and-Fibonacci-Polynomials-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.4 Fibonacci Numbers and Fibonacci Polynomials",
    "category": "section",
    "text": "Reduce.Algebra.fibonacciReduce.Algebra.fibonaccip"
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.motzkin",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.motzkin",
    "category": "function",
    "text": "motzkin(n)\n\nA Motzkin number M_n (named after Theodore Motzkin) is the number of different ways of drawing non-intersecting chords on a circle between n points. For a non-negative integer n, the operator motzkin(n) returns the nth Motzkin number, according to the recursion formula\n\nM_0 =  1  M_1 = 1  M_n+1  =  frac2n+3n+3M_n + frac3nn+3M_n-1\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#.5-Motzkin-numbers-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.5 Motzkin numbers",
    "category": "section",
    "text": "Reduce.Algebra.motzkin"
},

{
    "location": "man/07-prefix-ops.html#.6-CHANGEVAR-Operator-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.6 CHANGEVAR Operator",
    "category": "section",
    "text": "Not initially supported by Reduce.jl parser, see upstream docs for more information."
},

{
    "location": "man/07-prefix-ops.html#.7-CONTINUED_FRACTION-Operator-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.7 CONTINUED_FRACTION Operator",
    "category": "section",
    "text": "Not initially supported by Reduce.jl parser, see upstream docs for more information."
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.df",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.df",
    "category": "function",
    "text": "df(r...)\n\nThe operator df is used to represent partial differentiation with respect to one or more variables. It is used with the syntax:\n\nR\"df(⟨EXPRN:algebraic⟩[,⟨VAR:kernel⟩ <,⟨NUM:integer⟩ >])\"\n\nThe first argument is the expression to be differentiated. The remaining arguments specify the differentiation variables and the number of times they are applied.\n\nThe number num may be omitted if it is 1. For example,\n\nreduce> df(y,x)\n\nreduce> df(y,x,2)\n\nreduce> df(y,x1,2,x2,x3,2)\n\nThe evaluation of df(y,x) proceeds as follows: first, the values of y and x are found. Let us assume that x has no assigned value, so its value is x. Each term or other part of the value of y that contains the variable x is differentiated by the standard rules. If z is another variable, not x itself, then its derivative with respect to x is taken to be 0, unless z has previously been declared to depend on x, in which case the derivative is reported as the symbol df(z,x).\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#.8-DF-Operator-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.8 DF Operator",
    "category": "section",
    "text": "Reduce.Algebra.df"
},

{
    "location": "man/07-prefix-ops.html#.8.1-Switches-influencing-differentiation-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.8.1 Switches influencing differentiation",
    "category": "section",
    "text": "Consider df(u,x,y,z). If none of x,y,z are equal to u then the order of differentiation is commuted into a canonical form, unless the switch nocommutedf is turned on (default is off). if at least one of x,y,z is equal to u then the order of differentiation is not commuted and the derivative is not simplified to zero, unless the switch commutedf is turned on. It is off by default.If commutedf is off and the switch simpnoncomdf is on then simplify as follows:df(u,x,u)        ->  df(u,x,2) / df(u,x)  \ndf(u,x,n,u)      ->  df(u,x,n+1) / df(u,x)provided u depends only on the one variable x. This simplification removes the non-commutative aspect of the derivative.If the switch expanddf is turned on then REDUCE uses the chain rule to expand symbolic derivatives of indirectly dependent variables provided the result is unambiguous, i.e. provided there is no direct dependence. It is off by default. Thus, for example, givenjulia> Algebra.depend(:f,:u,:v); Algebra.depend((:u,:v),:x)\n\njulia> Algebra.on(:expanddf)\n\njulia> Algebra.df(:f,:x)\n:(df(f, u) * df(u, x) + df(f, v) * df(v, x))whereas afterjulia> Algebra.depend(:f,:x)df(:f,:x) does not expand at all (since the result would be ambiguous and the algorithm would loop).Turning on the switch allowdfint allows “differentiation under the integral sign”, i.e.df(int(y, x), v) -> int(df(y, v), x)if this results in a simplification. If the switch dfint is also turned on then this happens regardless of whether the result simplifies. Both switches are off by default."
},

{
    "location": "man/07-prefix-ops.html#.8.2-Adding-Differentiation-Rules-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.8.2 Adding Differentiation Rules",
    "category": "section",
    "text": "The let statement can be used to introduce rules for differentiation of user-defined operators. Its general form isR\"for all ⟨var1⟩,…,⟨varn⟩ let df(⟨operator⟩⟨varlist⟩,⟨vari⟩)=⟨expression⟩\"where ⟨varlist⟩ ::= (⟨var1⟩,…,⟨varn⟩), and ⟨var1⟩,…,⟨varn⟩ are the dummy variable arguments of ⟨operator⟩.An analogous form applies to infix operators.Examples:R\"for all x let df(tan x,x)= 1 + tan(x)^2\"(This is how the tan differentiation rule appears in the REDUCE source.)R\"for all x,y let df(f(x,y),x)=2*f(x,y),  df(f(x,y),y)=x*f(x,y)\"Notice that all dummy arguments of the relevant operator must be declared arbitrary by the for all command, and that rules may be supplied for operators with any number of arguments. If no differentiation rule appears for an argument in an operator, the differentiation routines will return as result an expression in terms of df. For example, if the rule for the differentiation with respect to the second argument of f is not supplied, the evaluation of df(f(x,z),z) would leave this expression unchanged. (No depend declaration is needed here, since f(x,z) obviously “depends on” z.)Once such a rule has been defined for a given operator, any future differentiation rules for that operator must be defined with the same number of arguments for that operator, otherwise we get the error messageERROR: Reduce: \nIncompatible df rule argument length for <operator>"
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.int",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.int",
    "category": "function",
    "text": "int(r...)\n\nint is an operator in REDUCE for indefinite integration using a combination of the Risch-Norman algorithm and pattern matching. It is used with the syntax:\n\nR\"int(⟨EXPRN:algebraic⟩,⟨VAR:kernel⟩)\"\n\nThis will return correctly the indefinite integral for expressions comprising polynomials, log functions, exponential functions and tan and atan. The arbitrary constant is not represented. If the integral cannot be done in closed terms, it returns a formal integral for the answer in one of two ways:\n\nIt returns the input, int(…,…) unchanged.\nIt returns an expression involving ints of some other functions (sometimes more complicated than the original one, unfortunately).\n\nRational functions can be integrated when the denominator is factorizable by the program. In addition it will attempt to integrate expressions involving error functions, dilogarithms and other trigonometric expressions. In these cases it might not always succeed in finding the solution, even if one exists.\n\nExamples:\n\njulia> Algebra.int(:(log(x)),:x)\n:((log(x) - 1) * x)\n\njulia> Algebra.int(:(e^x),:x)\n:(e ^ x)\n\nThe program checks that the second argument is a variable and gives an error if it is not.\n\nNote: If the int operator is called with 4 arguments, REDUCE will implicitly call the definite integration package (DEFINT) and this package will interpret the third and fourth arguments as the lower and upper limit of integration, respectively. For details, consult the documentation on the DEFINT package.\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#.9-INT-Operator-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.9 INT Operator",
    "category": "section",
    "text": "Reduce.Algebra.int"
},

{
    "location": "man/07-prefix-ops.html#.9.1-Options-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.9.1 Options",
    "category": "section",
    "text": "The switch trint when on will trace the operation of the algorithm. It produces a great deal of output in a somewhat illegible form, and is not of much interest to the general user. It is normally off.The switch trintsubst when on will trace the heuristic attempts to solve the integral by substitution. It is normally off.If the switch failhard is on the algorithm will terminate with an error if the integral cannot be done in closed terms, rather than return a formal integration form. failhard is normally off.The switch nolnr suppresses the use of the linear properties of integration in cases when the integral cannot be found in closed terms. It is normally off.The switch nointsubst disables the heuristic attempts to solve the integral by substitution. It is normally off."
},

{
    "location": "man/07-prefix-ops.html#.9.2-Advanced-Use-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.9.2 Advanced Use",
    "category": "section",
    "text": "If a function appears in the integrand that is not one of the functions exp, erf, tan, atan, log, dilog then the algorithm will make an attempt to integrate the argument if it can, differentiate it and reach a known function. However the answer cannot be guaranteed in this case. If a function is known to be algebraically independent of this set it can be flagged transcendental byR\"flag(’(trilog),’transcendental)\"in which case this function will be added to the permitted field descriptors for a genuine decision procedure. If this is done the user is responsible for the mathematical correctness of his actions.The standard version does not deal with algebraic extensions. Thus integration of expressions involving square roots and other like things can lead to trouble. A contributed package that supports integration of functions involving square roots is available, however (ALGINT, chapter 16.1). In addition there is a definite integration package, DEFINT( chapter 16.18)."
},

{
    "location": "man/07-prefix-ops.html#.9.3-References-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.9.3 References",
    "category": "section",
    "text": "A. C. Norman & P. M. A. Moore, “Implementing the New Risch Algorithm”, Proc. 4th International Symposium on Advanced Comp. Methods in Theor. Phys., CNRS, Marseilles, 1977.S. J. Harrington, “A New Symbolic Integration System in Reduce”, Comp. Journ. 22 (1979) 2.A. C. Norman & J. H. Davenport, “Symbolic Integration — The Dust Settles?”, Proc. EUROSAM 79, Lecture Notes in Computer Science 72, Springer-Verlag, Berlin Heidelberg New York (1979) 398-407."
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.length",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.length",
    "category": "function",
    "text": "length(r)\n\nlength is a generic operator for finding the length of various objects in the system. The meaning depends on the type of the object. In particular, the length of an algebraic expression is the number of additive top-level terms its expanded representation.\n\nExamples:\n\njulia> length(:(a+b))\n2\n\njulia> length(2)\n1\n\nOther objects that support a length operator include arrays, lists and matrices. The explicit meaning in these cases is included in the description of these objects.\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#.10-LENGTH-Operator-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.10 LENGTH Operator",
    "category": "section",
    "text": "Reduce.Algebra.length"
},

{
    "location": "man/07-prefix-ops.html#.11-MAP-Operator-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.11 MAP Operator",
    "category": "section",
    "text": "Not initially supported by Reduce.jl parser, see upstream docs for more information."
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.mkid",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.mkid",
    "category": "function",
    "text": "mkid(u,v)\n\nIn many applications, it is useful to create a set of identifiers for naming objects in a consistent manner. In most cases, it is sufficient to create such names from two components. The operator mkid is provided for this purpose. Its syntax is:\n\nR\"mkid(U:id,V:id|non-negative integer)\"\n\nfor example\n\njulia> Algebra.mkid(:a,3)\n:a3\n\njulia> Algebra.mkid(:apple,:s)\n:apples\n\nwhile mkid(:(a+b),2) gives an error.\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#.12-MKID-Operator-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.12 MKID Operator",
    "category": "section",
    "text": "Reduce.Algebra.mkidThe set statement can be used to give a value to the identifiers created by mkid, for examplejulia> Algebra.set(Algebra.mkid(:a,3),2)will give a3 the value 2. Similarly, the unset statement can be used to remove the value from these identifiers, for examplejulia> Algebra.unset(Algebra.mkid(:a,3))"
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.pochhammer",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.pochhammer",
    "category": "function",
    "text": "pochhammer(a,k)\n\nThe Pochhammer notation (a)_k (also called Pochhammer’s symbol) is supported by the binary operator pochhammer(a,k). For a non-negative integer k, it is defined as (http://dlmf.nist.gov/5.2.iii)\n\n(a)_0	= 1\n\n(a)_k	= a(a + 1)(a + 2)(a + k - 1)\n\nFor a = 012, this is equivalent to\n\n(a)k	= fracGamma (a+-k-)Gamma (a)\n\nWith rounded off, this expression is evaluated numerically if a and k are both integral, and otherwise may be simplified where appropriate. The simplification rules are based upon algorithms supplied by Wolfram Koepf.\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#.13-The-Pochhammer-Notation-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.13 The Pochhammer Notation",
    "category": "section",
    "text": "Reduce.Algebra.pochhammer"
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.pf",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.pf",
    "category": "function",
    "text": "pf(expr,var)\n\nR\"pf(⟨exp⟩,⟨var⟩)\" transforms the expression ⟨exp⟩ into a list of partial fractions with respect to the main variable, ⟨var⟩. pf does a complete partial fraction decomposition, and as the algorithms used are fairly unsophisticated (factorization and the extended Euclidean algorithm), the code may be unacceptably slow in complicated cases.\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#.14-PF-Operator-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.14 PF Operator",
    "category": "section",
    "text": "Reduce.Algebra.pfExample: Given R\"2/((x+1)^2*(x+2))\" in the workspace, R\"pf(ws,x)\" gives the result    2      - 2         2\n{-------,-------,--------------}\n  x + 2   x + 1    2\n                  x  + 2*x + 1If you want the denominators in factored form, use off(:exp). Thus, with R\"2/((x+1)^2*(x+2))\" in the workspace, the commands R\"off(exp); pf(ws,x)\" give the result    2      - 2       2\n{-------,-------,----------}\n  x + 2   x + 1          2\n                  (x + 1)To recombine the terms, for each… sum can be used. So with the above list in the workspace, R\"for each j in ws sum j\" returns the result        2\n------------------\n                2\n (x + 2)*(x + 1)Alternatively, one can use the operations on lists to extract any desired term."
},

{
    "location": "man/07-prefix-ops.html#.15-SELECT-Operator-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.15 SELECT Operator",
    "category": "section",
    "text": "Not initially supported by Reduce.jl parser, see upstream docs for more information."
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.solve",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.solve",
    "category": "function",
    "text": "solve(r...)\n\nsolve is an operator for solving one or more simultaneous algebraic equations. It is used with the syntax:\n\nR\"SOLVE(⟨EXPRN:algebraic⟩[,⟨VAR:kernel⟩∣,⟨VARLIST:list of kernels⟩])\"\n\nexprn is of the form ⟨expression⟩ or {⟨expression1⟩,⟨expression2⟩, …}. Each expression is an algebraic equation, or is the difference of the two sides of the equation. The second argument is either a kernel or a list of kernels representing the unknowns in the system. This argument may be omitted if the number of distinct, non-constant, top-level kernels equals the number of unknowns, in which case these kernels are presumed to be the unknowns.\n\nFor one equation, solve recursively uses factorization and decomposition, together with the known inverses of log, sin, cos, ^, acos, asin, and linear, quadratic, cubic, quartic, or binomial factors. Solutions of equations built with exponentials or logarithms are often expressed in terms of Lambert’s W function. This function is (partially) implemented in the special functions package.\n\nLinear equations are solved by the multi-step elimination method due to Bareiss, unless the switch cramer is on, in which case Cramer’s method is used. The Bareiss method is usually more efficient unless the system is large and dense.\n\nNon-linear equations are solved using the Groebner basis package (chapter 16.28). Users should note that this can be quite a time consuming process.\n\nExamples:\n\nAlgebra.solve(:(log(sin(x+3))^5 == 8),:x)\nAlgebra.solve(:(a*log(sin(x+3))^5 - b), :(sin(x+3)))\nAlgebra.solve((:(a*x+y==3),:(y=-2)),(:x,:y))\n\nsolve returns a list of solutions. If there is one unknown, each solution is an equation for the unknown. If a complete solution was found, the unknown will appear by itself on the left-hand side of the equation. On the other hand, if the solve package could not find a solution, the “solution” will be an equation for the unknown in terms of the operator root_of. If there are several unknowns, each solution will be a list of equations for the unknowns. For example,\n\njulia> Algebra.solve(:(x^2==1),:x)\n(:(x = 1), :(x = -1))\n\njulia> Algebra.solve(:(x^7-x^6+x^2==1),:x)\n(:(x = root_of(x_ ^ 6 + x_ + 1, x_, tag_1)), :(x = 1))\n\njulia> Algebra.solve((:(x+3y==7),:(y-x==1)),(:x,:y))\n(:(x = 1), :(y = 2))\n\nThe tag argument is used to uniquely identify those particular solutions. ```\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.root_multiplicities",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.root_multiplicities",
    "category": "function",
    "text": "root_multiplicities()\n\nSolution multiplicities are stored in the global variable root_multiplicities rather than the solution list. The value of this variable is a list of the multiplicities of the solutions for the last call of solve. For example,\n\njulia> Algebra.solve(:(x^2==2x-1),:x); Algebra.root_multiplicities()\n\ngives the results\n\n(:(x = 1),)\n \n(2,)\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#Reduce.multiplicities",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.multiplicities",
    "category": "function",
    "text": "multiplicities(::Bool)\n\nIf you want the multiplicities explicitly displayed, the switch multiplicities can be turned on. For example\n\njulia> Algebra.on(:multiplicities); Algebra.solve(:(x^2==2x-1),:x)\n\nyields the result ```Julia (:(x = 1), :(x = 1))\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#.16-SOLVE-Operator-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.16 SOLVE Operator",
    "category": "section",
    "text": "Reduce.Algebra.solveReduce.Algebra.root_multiplicitiesReduce.multiplicities"
},

{
    "location": "man/07-prefix-ops.html#.16.1-Handling-of-Undetermined-Solutions-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.16.1 Handling of Undetermined Solutions",
    "category": "section",
    "text": "When solve cannot find a solution to an equation, it normally returns an equation for the relevant indeterminates in terms of the operator root_of. For example, the expressionjulia> Algebra.solve(:(cos(x)+log(x)),:x)returns the result(:(x = root_of(cos(x_) + log(x_), x_, tag_1)),)An expression with a top-level root_of operator is implicitly a list with an unknown number of elements (since we don’t always know how many solutions an equation has). If a substitution is made into such an expression, closed form solutions can emerge. If this occurs, the root_of construct is replaced by an operator one_of. At this point it is of course possible to transform the result of the original solve operator expression into a standard solve solution. To effect this, the operator expand_cases can be used.The following example shows the use of these facilities:julia> Algebra.solve(:(-a*x^3+a*x^2+x^4-x^3-4*x^2+4),:x)\n(:(x = root_of((a * x_ ^ 2 - x_ ^ 3) + 4x_ + 4, x_, tag_2)), :(x = 1))\n\njulia> Algebra.sub(:a=-1,ans)\n(:(x=one_of((2, -1, -2), tag_2)), :(x=1))\n \njulia> Algebra.expand_cases(ans)\n(:(x=2), :(x=-1), :(x=-2), :(x=1))"
},

{
    "location": "man/07-prefix-ops.html#.16.2-Solutions-of-Equations-Involving-Cubics-and-Quartics-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.16.2 Solutions of Equations Involving Cubics and Quartics",
    "category": "section",
    "text": "Since roots of cubics and quartics can often be very messy, a switch fullroots is available, that, when off (the default), will prevent the production of a result in closed form. The root_of construct will be used in this case instead.In constructing the solutions of cubics and quartics, trigonometrical forms are used where appropriate. This option is under the control of a switch trigform, which is normally on.The following example illustrates the use of these facilities:julia> Algebra.rlet(:xx => :(solve(x^3+x+1,x)))\n \njulia> rcall(:xx)\n(:(x = root_of(x_ ^ 3 + x_ + 1, x_, tag_1)),)\n\njulia> Algebra.on(:fullroots)\n\njulia> collect(rcall(:xx))\n3-element Array{Expr,1}:\n :(x = -((sqrt(3) * cosh(asinh((3 * sqrt(3)) // 2) // 3) * im - sinh(asinh((3 * sqrt(3)) // 2) // 3))) // sqrt(3))\n :(x = (sqrt(3) * cosh(asinh((3 * sqrt(3)) // 2) // 3) * im + sinh(asinh((3 * sqrt(3)) // 2) // 3)) // sqrt(3))\n :(x = (-2 * sinh(asinh((3 * sqrt(3)) // 2) // 3)) // sqrt(3))\n\njulia> off(:trigform)\n \njulia> rcall(R\"xx\")\n                             2/3  \n{x=( - (sqrt(31) - 3*sqrt(3))   *sqrt(3)*i  \n \n                             2/3    2/3  \n     - (sqrt(31) - 3*sqrt(3))    - 2   *sqrt(3)*i  \n \n        2/3                           1/3  1/3  \n     + 2   )/(2*(sqrt(31) - 3*sqrt(3))   *6  \n \n                1/6  \n              *3   ),  \n \n                          2/3  \n x=((sqrt(31) - 3*sqrt(3))   *sqrt(3)*i  \n \n                             2/3    2/3  \n     - (sqrt(31) - 3*sqrt(3))    + 2   *sqrt(3)*i  \n \n        2/3                           1/3  1/3  \n     + 2   )/(2*(sqrt(31) - 3*sqrt(3))   *6  \n \n                1/6  \n              *3   ),  \n \n                           2/3    2/3  \n     (sqrt(31) - 3*sqrt(3))    - 2  \n x=-------------------------------------}  \n                          1/3  1/3  1/6  \n    (sqrt(31) - 3*sqrt(3))   *6   *3"
},

{
    "location": "man/07-prefix-ops.html#.16.3-Other-Options-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.16.3 Other Options",
    "category": "section",
    "text": "If solvesingular is on (the default setting), degenerate systems such as x+y=0, 2x+2y=0 will be solved by introducing appropriate arbitrary constants. The consistent singular equation 0=0 or equations involving functions with multiple inverses may introduce unique new indeterminant kernels arbcomplex(j), or arbint(j), (j=12), representing arbitrary complex or integer numbers respectively. To automatically select the principal branches, do off(:allbranch). To avoid the introduction of new indeterminant kernels do off(:arbvars) – then no equations are generated for the free variables and their original names are used to express the solution forms. To suppress solutions of consistent singular equations do off(:solvesingular).To incorporate additional inverse functions do, for example:R\"put(’sinh,’inverse,’asinh)\"\nR\"put(’asinh,’inverse,’sinh)\"together with any desired simplification rules such asR\"for all x let sinh(asinh(x))=x, asinh(sinh(x))=x\"For completeness, functions with non-unique inverses should be treated as ^, sin, and cos are in the solve module source.Arguments of asin and acos are not checked to ensure that the absolute value of the real part does not exceed 1; and arguments of log are not checked to ensure that the absolute value of the imaginary part does not exceed π; but checks (perhaps involving user response for non-numerical arguments) could be introduced using let statements for these operators."
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.requirements",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.requirements",
    "category": "function",
    "text": "requirements()\n\nThe proper design of a variable sequence supplied as a second argument to solve is important for the structure of the solution of an equation system. Any unknown in the system not in this list is considered totally free. E.g. the call\n\nAlgebra.solve((:(x==2z),:(z==2y)),(:z,))\n\nproduces an empty list as a result because there is no function z = z(x,y) which fulfills both equations for arbitrary x and y values. In such a case the share variable requirements displays a set of restrictions for the parameters of the system:\n\njulia> Algebra.requirements()\n(:(x - 4y),)\n\nThe non-existence of a formal solution is caused by a contradiction which disappears only if the parameters of the initial system are set such that all members of the requirements list take the value zero. For a linear system the set is complete: a solution of the requirements list makes the initial system solvable. E.g. in the above case a substitution x = 4y makes the equation set consistent. For a non-linear system only one inconsistency is detected. If such a system has more than one inconsistency, you must reduce them one after the other. 1 The set shows you also the dependency among the parameters: here one of x and y is free and a formal solution of the system can be computed by adding it to the variable list of solve. The requirement set is not unique – there may be other such sets.\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.assumptions",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.assumptions",
    "category": "function",
    "text": "assumptions()\n\nA system with parameters may have a formal solution, e.g. \n\njulia> Algebra.solve((:(x==a*z+1),:(0==b*z-y)),(:z,:x))\n(:(z = y // b), :(x = (a * y + b) // b))\n\nwhich is not valid for all possible values of the parameters. The variable assumptions contains then a list of restrictions: the solutions are valid only as long as none of these expressions vanishes. Any zero of one of them represents a special case that is not covered by the formal solution. In the above case the value is\n\njulia> Algebra.assumptions()\n(:b,)\n\nwhich excludes formally the case b = 0; obviously this special parameter value makes the system singular. The set of assumptions is complete for both, linear and non–linear systems.\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#.16.4-Parameters-and-Variable-Dependency-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.16.4 Parameters and Variable Dependency",
    "category": "section",
    "text": "Reduce.Algebra.requirementsReduce.Algebra.assumptionssolve rearranges the variable sequence to reduce the (expected) computing time. This behavior is controlled by the switch varopt, which is on by default. If it is turned off, the supplied variable sequence is used or the system kernel ordering is taken if the variable list is omitted. The effect is demonstrated by an example:julia> rcall(R\"s:= {y^3+3x=0,x^2+y^2=1}\");\n \njulia> Algebra.solve(R\"s\",(:y,:x))\n \n              6       2\n{{y=root_of(y_  + 9*y_  - 9,y_,tag_2),\n\n         3\n      - y\n  x=-------}}\n       3\n\njulia> Algebra.off(:varopt); Algebra.solve(:s,(:y,:x)) |> collect\n2-element Array{Expr,1}:\n :(y = (-(((x ^ 4 - 2 * x ^ 2) + 10)) * x) // 3)                     \n :(x = root_of(((x_ ^ 6 - 3 * x_ ^ 4) + 12 * x_ ^ 2) - 1, x_, tag_3))In the first case, solve forms the solution as a set of pairs (y_ix(y_i)) because the degree of x is higher – such a rearrangement makes the internal computation of the Gröbner basis generally faster. For the second case the explicitly given variable sequence is used such that the solution has now the form (x_i,y(x_i)). Controlling the variable sequence is especially important if the system has one or more free variables. As an alternative to turning off varopt, a partial dependency among the variables can be declared using the depend statement: solve then rearranges the variable sequence but keeps any variable ahead of those on which it depends.julia> Algebra.on(:varopt)\n\njulia> rcall(R\"s:={a^3+b,b^2+c}\");\n\njulia> Algebra.solve(:s,(:a,:b,:c))\n(:(a = arbcomplex(1)), :(b = -(a ^ 3)), :(c = -(a ^ 6))) \n\njulia> Algebra.depend(:a,:c); Algebra.depend(:b,:c)\n\njulia> Algebra.solve(:s,(:a,:b,:c))\n3-element Array{Expr,1}:\n :(c = arbcomplex(2))                  \n :(a = root_of(a_ ^ 6 + c, a_, tag_3))\n :(b = -(a ^ 3)) Here solve is forced to put c after a and after b, but there is no obstacle to interchanging a and b."
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.even",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.even",
    "category": "function",
    "text": "even(r...)\n\nAn operator can be declared to be even in its first argument by the declarations even. Expressions involving an operator declared in this manner are transformed if the first argument contains a minus sign. Any other arguments are not affected. For example, the declaration\n\njulia> Algebra.even(:f1)\n\nmeans that\n\n        f1(-a)    ->    f1(a)  \n        f1(-a,-b) ->    f1(a,-b)  \n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.odd",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.odd",
    "category": "function",
    "text": "odd(r...)\n\nAn operator can be declared to be odd in its first argument by the declarations odd. Expressions involving an operator declared in this manner are transformed if the first argument contains a minus sign. Any other arguments are not affected. In addition, if say f is declared odd, then f(0) is replaced by zero unless f is also declared non zero by the declaration nonzero. For example, the declarations\n\njulia> Algebra.odd(:f2)\n\nmeans that\n\n        f2(-a)    ->   -f2(a)  \n        f2(0)     ->    0\n\nTo inhibit the last transformation, say nonzero(:f2).\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#.17-Even-and-Odd-Operators-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.17 Even and Odd Operators",
    "category": "section",
    "text": "Reduce.Algebra.evenReduce.Algebra.odd"
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.linear",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.linear",
    "category": "function",
    "text": "linear(r...)\n\nAn operator can be declared to be linear in its first argument over powers of its second argument. If an operator f is so declared, f of any sum is broken up into sums of fs, and any factors that are not powers of the variable are taken outside. This means that f must have (at least) two arguments. In addition, the second argument must be an identifier (or more generally a kernel), not an expression.\n\nExample: If f were declared linear, then\n\nf(a*x^5+b*x+c,x) ->  f(x^5,x)*a + f(x,x)*b + f(1,x)*c\n\nMore precisely, not only will the variable and its powers remain within the scope of the f operator, but so will any variable and its powers that had been declared to depend on the prescribed variable; and so would any expression that contains that variable or a dependent variable on any level, e.g. cos(sin(x)).\n\nTo declare operators f and g to be linear operators, use:\n\njulia> Algebra.linear(:f,:g)\n\nThe analysis is done of the first argument with respect to the second; any other arguments are ignored. It uses the following rules of evaluation:\n\nf(0) 		-> 0\nf(-y,x) 	-> -f(y,x)\nf(y+z,x) 	-> f(y,x)+f(z,x)\nf(y*z,x) 	-> z*f(y,x)   	if z does not depend on x\nf(y/z,x) 	-> f(y,x)/z	if z does not depend on x\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#.18-Linear-Operators-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.18 Linear Operators",
    "category": "section",
    "text": "Reduce.Algebra.linearTo summarize, y “depends” on the indeterminate x in the above if either of the following hold:y is an expression that contains x at any level as a variable, e.g.: cos(sin(x))\nAny variable in the expression y has been declared dependent on x by use of the declaration depend.The use of such linear operators can be seen in the paper Fox, J.A. and A. C. Hearn, “Analytic Computation of Some Integrals in Fourth Order Quantum Electrodynamics” Journ. Comp. Phys. 14 (1974) 301-317, which contains a complete listing of a program for definite integration of some expressions that arise in fourth order quantum electrodynamics."
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.noncom",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.noncom",
    "category": "function",
    "text": "noncom(r...)\n\nAn operator can be declared to be non-commutative under multiplication by the declaration noncom.\n\nExample: After the declaration\n\njulia> Algebra.noncom(:u,:v);\n\nthe expressions u(x)*u(y)-u(y)*u(x) and u(x)*v(y)-v(y)*u(x) will remain unchanged on simplification, and in particular will not simplify to zero.\n\nNote that it is the operator (u and v in the above example) and not the variable that has the non-commutative property.\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#.19-Non-Commuting-Operators-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.19 Non-Commuting Operators",
    "category": "section",
    "text": "Reduce.Algebra.noncomThe let statement may be used to introduce rules of evaluation for such operators. In particular, the boolean operator ordp is useful for introducing an ordering on such expressions.Example: The ruleR\"for all x,y such that x neq y and ordp(x,y) let u(x)*u(y)= u(y)*u(x)+comm(x,y)\"would introduce the commutator of u(x) and u(y) for all x and y. Note that since ordp(x,x) is true, the equality check is necessary in the degenerate case to avoid a circular loop in the rule."
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.symmetric",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.symmetric",
    "category": "function",
    "text": "symmetric(r...)\n\nAn operator can be declared to be symmetric with respect to its arguments by the declaration symmetric. For example\n\njulia> Algebra.symmetric(:u,:v);\n\nmeans that any expression involving the top level operators u or v will have its arguments reordered to conform to the internal order used by REDUCE. The user can change this order for kernels by the command korder. For example, u(x,v(1,2)) would become u(v(2,1),x), since numbers are ordered in decreasing order, and expressions are ordered in decreasing order of complexity.\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.antisymmetric",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.antisymmetric",
    "category": "function",
    "text": "antisymmetric(r...)\n\nthe declaration antisymmetric declares an operator antisymmetric. For example,\n\njulia> Algebra.antisymmetric(:l,:m);\n\nmeans that any expression involving the top level operators l or m will have its arguments reordered to conform to the internal order of the system, and the sign of the expression changed if there are an odd number of argument interchanges necessary to bring about the new order.\n\nFor example, l(x,m(1,2)) would become -l(-m(2,1),x) since one interchange occurs with each operator. An expression like l(x,x) would also be replaced by 0.\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#.20-Symmetric-and-Antisymmetric-Operators-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.20 Symmetric and Antisymmetric Operators",
    "category": "section",
    "text": "Reduce.Algebra.symmetricReduce.Algebra.antisymmetric"
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.operator",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.operator",
    "category": "function",
    "text": "operator(r...)\n\nThe user may add new prefix operators to the system by using the declaration operator. For example:\n\njulia> Algebra.operator(:h,:g1,:arctan)\n\nadds the prefix operators h, g1 and arctan to the system.\n\nThis allows symbols like h(w), h(x,y,z), g1(p+q), arctan(u/v) to be used in expressions, but no meaning or properties of the operator are implied. The same operator symbol can be used equally well as a 0-, 1-, 2-, 3-, etc.-place operator.\n\nTo give a meaning to an operator symbol, or express some of its properties, let statements can be used, or the operator can be given a definition as a procedure.\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#.21-Declaring-New-Prefix-Operators-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.21 Declaring New Prefix Operators",
    "category": "section",
    "text": "Reduce.Algebra.operatorIf the user forgets to declare an identifier as an operator, the system will prompt the user to do so in interactive mode, or do it automatically in non-interactive mode. A diagnostic message will also be printed if an identifier is declared operator more than once.Operators once declared are global in scope, and so can then be referenced anywhere in the program. In other words, a declaration within a block (or a procedure) does not limit the scope of the operator to that block, nor does the operator go away on exiting the block (use clear instead for this purpose)."
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.infix",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.infix",
    "category": "function",
    "text": "infix(r...)\n\nUsers can add new infix operators by using the declarations infix and precedence. For example,\n\njulia> Algebra.infix(:mm)\n\nThe declaration infix(:mm) would allow one to use the symbol mm as an infix operator: R\"a mm b\" instead of R\"mm(a,b)\".\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.precedence",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.precedence",
    "category": "function",
    "text": "precedence(a,b)\n\nUsers can add new infix operators by using the declarations infix and precedence. For example,\n\njulia> Algebra.precedence(:mm,:-)\n\nThe declaration precedence(:mm,:-) says that mm should be inserted into the infix operator precedence list just after the - operator. This gives it higher precedence than - and lower precedence than * . Thus R\"a - b mm c - d\" means R\"a - (b mm c) - d\", while R\"a * b mm c * d\" means R\"(a * b) mm (c * d)\".\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#.22-Declaring-New-Infix-Operators-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.22 Declaring New Infix Operators",
    "category": "section",
    "text": "Reduce.Algebra.infixReduce.Algebra.precedenceBoth infix and prefix operators have no transformation properties unless let statements or procedure declarations are used to assign a meaning.We should note here that infix operators so defined are always binary: R\"a mm b mm c\" means R\"(a mm b) mm c\"."
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.depend",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.depend",
    "category": "function",
    "text": "depend(r...)\n\nThere are several facilities in REDUCE, such as the differentiation operator and the linear operator facility, that can utilize knowledge of the dependency between various variables, or kernels. Such dependency may be expressed by the command depend. This takes an arbitrary number of arguments and sets up a dependency of the first argument on the remaining arguments. For example,\n\njulia> Algebra.depend(:x,:y,:z)\n\nsays that x is dependent on both y and z.\n\njulia> Algebra.depend(:z,:(cos(x)),:y)\n\nsays that z is dependent on cos(x) and y.\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#Reduce.Algebra.nodepend",
    "page": "7 Built-in Prefix Operators",
    "title": "Reduce.Algebra.nodepend",
    "category": "function",
    "text": "nodepend(r...)\n\nDependencies introduced by depend can be removed by nodepend. The arguments of this are the same as for depend. For example, given the above dependencies,\n\njulia> Algebra.nodepend(:z,:(cos(x)))\n\nsays that z is no longer dependent on cos(x), although it remains dependent on y.\n\n\n\n"
},

{
    "location": "man/07-prefix-ops.html#.23-Creating/Removing-Variable-Dependency-1",
    "page": "7 Built-in Prefix Operators",
    "title": "7.23 Creating/Removing Variable Dependency",
    "category": "section",
    "text": "Reduce.Algebra.dependReduce.Algebra.nodepend"
},

{
    "location": "man/20-maintaining.html#",
    "page": "20 Maintaining REDUCE",
    "title": "20 Maintaining REDUCE",
    "category": "page",
    "text": ""
},

{
    "location": "man/20-maintaining.html#Maintaining-REDUCE-1",
    "page": "20 Maintaining REDUCE",
    "title": "20 Maintaining REDUCE",
    "category": "section",
    "text": "Since January 1, 2009 REDUCE is Open Source Software. It is hosted athttp://reduce-algebra.sourceforge.net/We mention here three ways in which REDUCE is maintained. The first is the collection of queries, observations and bug-reports. All users are encouraged to subscribe to the mailing list that Sourceforge.net provides so that they will receive information about updates and concerns. Also on SourceForge there is a bug tracker and a forum. The expectation is that the maintainers and keen users of REDUCE will monitor those and try to respond to issues. However these resources are not there to seek answers to Maths homework problems - they are intended specifically for issues to do with the use and support of REDUCE.The second level of support is provided by the fact that all the sources of REDUCE are available, so any user who is having difficulty either with a bug or understanding system behaviour can consult the code to see if (for instance) comments in it clarify something that was unclear from the regular documentation.The source files for REDUCE are available on SourceForge in the Subversion repository. Check the \"code/SVN\" tab on the SourceForge page to find instructions for using a Subversion client to fetch the most up to date copy of everything. From time to time there may be one-file archives of a snapshot of the sources placed in the download area on SourceForge, and eventually some of these mat be marked as “stable” releases, but at present it is recommended that developers use a copy from the Subversion repository.The files fetched there come with a directory called “trunk” that holds the main current REDUCE, and one called “branches” that is reserved for future experimental versions. All the files that we have for creating help files and manuals should also be present in the files you fetch.The packages that make up the source for the algebraic capabilities of REDUCE are in the “packages” sub-directory, and often there are test files for a package present there and especially for contributed packages there will be documentation in the form of a LATEX file. Although REDUCE is coded in its own language many people in the past have found that it does not take too long to start to get used to it.In various cases even fairly “ordinary end users” may wish to fetch the source version of REDUCE and compile it all for themselves. This may either be because they need the benefit of a bug-fix only recently checked into the subversion repository or because no pre-compiled binary is available for the particular computer and operating system they use. This latter is to some extent unavoidable since REDUCE can run on both 32 and 64-bit Windows, the various MacOSX options (eg Intel and Powerpc), many different distributions of Linux, some BSD variants and Solaris (at least). It is not practically feasible for us to provide a constant stream of up to date ready-built binaries for all these.There are instructions for compiling REDUCE present at the top of the trunk source tree. Usually the hardest issue seems to be encuring that your computer has an adequate set of development tools and libraries installled before you start, but once that is sorted out the hope is that the compilation of REDUCE should proceed uneventfully if sometimes tediously.In a typical Open Source way the hope is that some of those who build REDUCE from source or explore the source (out of general interest or to pursue an understanding of some bug or detail) will transform themselves into contributors or developers which moves on to the third level of support.At this third level any user can contribute proposals for bug fixes or extensions to REDUCE or its documentation. It might be valuable to collect a library of additional user-contributed examples illustrating the use of the system too. To do this first ensure that you have a fully up to date copy of the sources from Subversion, and then depending on just what sort of change is being proposed provide the updates to the developers via the SourceForge bug tracker or other route. In time we may give more concrete guidance about the format of changes that will be easiest to handle. It is obviously important that proposed changes have been properly tested and that they are accompanied with a clear explanation of why they are of benefit. A specific concern here is that in the past fixes to a bug in one part of REDUCE have had bad effects on some other applications and packages, so some degree of caution is called for. Anybody who develops a significant whole new package for REDUCE is encouraged to make the developers aware so that it can be considered for inclusion.So the short form explanation about Support and Maintenance is that it is mainly focussed around the SourceForge system. That if discussions about bugs, requirements or issues are conducted there then all users and potential users of REDUCE will be able to benefit from reviewing them, and the Sourceforge mailing lists, tracker, forums and wiki will grow to be both a static repository of answers to common questions, an active set of locations to to get new issues looked at and a focus for guiding future development."
},

{
    "location": "man/A-reserved.html#",
    "page": "Appendix A: Reserved Identifiers",
    "title": "Appendix A: Reserved Identifiers",
    "category": "page",
    "text": ""
},

{
    "location": "man/A-reserved.html#Appendix-A:-Reserved-Identifiers-1",
    "page": "Appendix A: Reserved Identifiers",
    "title": "Appendix A: Reserved Identifiers",
    "category": "section",
    "text": "We list here all identifiers that are normally reserved in REDUCE including names of commands, operators and switches initially in the system. Excluded are words that are reserved in specific implementations of the system.Commandsalgebraic antisymmetric array bye clear clearrules comment cont decompose define depend display ed editdef end even factor for forall foreach go goto if in index infix input integer korder let linear lisp listargp load load_package mass match matrix matrixproc mshell nodepend noncom nonzero nospur odd off on operator order out pause precedence print_precision procedure quit real remember remfac remind retry return saveas scalar setmod share showtime shut spur symbolic symmetric unset vecdim vector weight write wtlevelBoolean Operatorsevenp fixp freeof numberp ordp primepInfix Operators:= = >= > <= < => + - * / // ^ ** . .. where setq or and member memq equal neq eq geq greaterp leq lessp plus difference minus times quotient recip expt consNumerical Operatorsabs acos acosh acot acoth acsc acsch airy_ai airy_aiprime airy_bi airy_biprime asec asech asin asinh atan atanh atan2 bernoulli besseli besselj besselk bessely beta cos cosh cot coth csc csch csch exp factorial fix floor gamma hankel1 hankel2 hypot ibeta igamma kummerm kummeru lerch_phi ln log logb log10 lommel1 lommel2 nextprime pochhammer polygamma psi round sec sech sin sinh sqrt struveh struvel tan tanh whittakerm whittakeru zetaPrefix Operatorsappend arbcomplex arbint arglength ceiling ci coeff coeffn cofactor conj continued_fraction deg den det df dilog ei eps erf expand_cases factorize fibonacci fibonaccip first gcd g hypergeometric impart int interpol lcm lcof length lhs linelength list lpower lterm mainvar map mat mateigen max meijerg min mkid motzkin nullspace num one_of part pf precision prod random random_new_seed rank rederr reduct remainder repart rest resultant reverse rhs root_of root_val second select set showrules si sign solve solidharmonicy sphericalharmonicy structr sub sum third totaldeg tp trace varnameReserved Variables_line_ assumptions card_no catalan e euler_gamma eval_mode fort_width golden_ratio high_pow i infinity k!* khinchin low_pow negative nil pi positive requirements root_multiplicities tSwitchesadjprec algint allbranch allfac allowdfint arbvars balance_mod bezout bfspace combineexpt combinelogs commutedf comp complex cramer cref defn demo dfint div echo errcont evallhseqp exp expanddf expandlogs ezgcd factor failhard fort fortupper fullroots gcd ifactor int intstr lcm list listargs mcd modular msg multiplicities nat nero nocommutedf noconvert nolnr nosplit output period precise precise_complex pret pri rat ratarg rational rationalize ratpri revpri rlisp88 roundall roundbf rounded savestructr simpnoncomdf solvesingular time tra trdefint trfac trigform trint varoptOtherReservedIdsbegin do then expr fexpr input lambda lisp macro product repeat smacro sum then until when while ws"
},

{
    "location": "man/B-bibliography.html#",
    "page": "Appendix B: Bibliography",
    "title": "Appendix B: Bibliography",
    "category": "page",
    "text": ""
},

{
    "location": "man/B-bibliography.html#Appendix-B:-Bibliography-1",
    "page": "Appendix B: Bibliography",
    "title": "Appendix B: Bibliography",
    "category": "section",
    "text": "[1] Sandra Fillebrown. Faster computation of bernoulli numbers. Journal of Algorithms, 13:431–445, 1992.[2] Wolfram Koepf, Power Series in Computer Algebra, J. Symbolic Computation 13 (1992)"
},

{
    "location": "man/C-changelog.html#",
    "page": "Appendix C: Changes since Version 3.8",
    "title": "Appendix C: Changes since Version 3.8",
    "category": "page",
    "text": ""
},

{
    "location": "man/C-changelog.html#Appendix-C:-Changes-since-Version-3.8-1",
    "page": "Appendix C: Changes since Version 3.8",
    "title": "Appendix C: Changes since Version 3.8",
    "category": "section",
    "text": "New packages assert bibasis breduce cde cdiff clprl gcref guardian lalr lessons libreduce listvecops lpdo redfront reduce4 sstools utf8Core package rlisp Support for namespaces (::)Default value in switch statementSupport for utf8 charactersCore package poly Improvements for differentiation: new switches expanddf, allowdfint etc (from odesolve)Core package alg New switch precise_complexImprovements for switch combineexpt (exptchk.red)New command unsetNew operators continued_fraction, totaldegOperators now defined in the REDUCE core:changevar, si, ci, gamma, igamma, psi, polygamma, beta, ibeta, euler, bernoulli, pochhammer, lerch_phi, polylog, zeta, besselj, bessely, besseli, besselk, hankel1, hankel2, kummerM, kummerU, struveh, struvel, lommel1, lommel2, whittakerm, whittakerw, Airy_Ai, Airy_Bi, Airy_AiPrime, Airy_biprime, binomial, solidharmonic, sphericalharmonic, fibonacci,fibonaccip, motzkin, hypergeometric, MeijerG.Constants now part of the core:now known as part of the core, as well as constants catalan, euler_gamma, golden_ratio, khinchin.Core Package solve New boolean operator polyp(p,var), to determine whether p is a pure polynomial in var, ie. the coefficients of p do not contain var.Core Package matrix New keyword matrixproc for declaration of matrix-valued procedures.Package defint Added trdefint switch for tracing."
},

{
    "location": "man/index.html#",
    "page": "Index",
    "title": "Index",
    "category": "page",
    "text": ""
},

{
    "location": "man/index.html#Index-1",
    "page": "Index",
    "title": "Index",
    "category": "section",
    "text": "List of documented methods in Reduce.jl. See upstream index for others."
},

]}
