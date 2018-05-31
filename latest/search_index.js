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
    "location": "man/08-display.html#",
    "page": "8 Display and Structuring of Expressions",
    "title": "8 Display and Structuring of Expressions",
    "category": "page",
    "text": ""
},

{
    "location": "man/08-display.html#Display-and-Structuring-of-Expressions-1",
    "page": "8 Display and Structuring of Expressions",
    "title": "8 Display and Structuring of Expressions",
    "category": "section",
    "text": "In this section, we consider a variety of commands and operators that permit the user to obtain various parts of algebraic expressions and also display their structure in a variety of forms. Also presented are some additional concepts in the REDUCE design that help the user gain a better understanding of the structure of the system.Pages = [\"08-display.md\"]"
},

{
    "location": "man/08-display.html#.1-Kernels-1",
    "page": "8 Display and Structuring of Expressions",
    "title": "8.1 Kernels",
    "category": "section",
    "text": "REDUCE is designed so that each operator in the system has an evaluation (or simplification) function associated with it that transforms the expression into an internal canonical form. This form, which bears little resemblance to the original expression, is described in detail in Hearn, A. C., “REDUCE 2: A System and Language for Algebraic Manipulation,” Proc. of the Second Symposium on Symbolic and Algebraic Manipulation, ACM, New York (1971) 128-133.The evaluation function may transform its arguments in one of two alternative ways. First, it may convert the expression into other operators in the system, leaving no functions of the original operator for further manipulation. This is in a sense true of the evaluation functions associated with the operators +, * and / , for example, because the canonical form does not include these operators explicitly. It is also true of an operator such as the determinant operator det because the relevant evaluation function calculates the appropriate determinant, and the operator det no longer appears. On the other hand, the evaluation process may leave some residual functions of the relevant operator. For example, with the operator cos, a residual expression like cos(x) may remain after evaluation unless a rule for the reduction of cosines into exponentials, for example, were introduced. These residual functions of an operator are termed kernels and are stored uniquely like variables. Subsequently, the kernel is carried through the calculation as a variable unless transformations are introduced for the operator at a later stage.In those cases where the evaluation process leaves an operator expression with non-trivial arguments, the form of the argument can vary depending on the state of the system at the point of evaluation. Such arguments are normally produced in expanded form with no terms factored or grouped in any way. For example, the expression cos(2*x+2*y) will normally be returned in the same form. If the argument 2*x+2*y were evaluated at the top level, however, it would be printed as 2*(x+y). If it is desirable to have the arguments themselves in a similar form, the switch intstr (for “internal structure”), if on, will cause this to happen. In cases where the arguments of the kernel operators may be reordered, the system puts them in a canonical order, based on an internal intrinsic ordering of the variables. However, some commands allow arguments in the form of kernels, and the user has no way of telling what internal order the system will assign to these arguments. To resolve this difficulty, we introduce the notion of a kernel form as an expression that transforms to a kernel on evaluation.Examples of kernel forms are:R\"a\"\nR\"cos(x*y)\"\nR\"log(sin(x))\"whereasR\"a*b\"\nR\"(a+b)^4\"are not.We see that kernel forms can usually be used as generalized variables, and most algebraic properties associated with variables may also be associated with kernels."
},

{
    "location": "man/08-display.html#Reduce.Algebra.ws",
    "page": "8 Display and Structuring of Expressions",
    "title": "Reduce.Algebra.ws",
    "category": "function",
    "text": "ws()\n\nSeveral mechanisms are available for saving and retrieving previously evaluated expressions. The simplest of these refers to the last algebraic expression simplified. When an assignment of an algebraic expression is made, or an expression is evaluated at the top level, (i.e., not inside a compound statement or procedure) the results of the evaluation are automatically saved in a variable ws that we shall refer to as the workspace. (More precisely, the expression is assigned to the variable ws that is then available for further manipulation.)\n\nExample: If we evaluate the expression (x+y)^2 at the top level and next wish to differentiate it with respect to y, we can simply say julia Algebradf(Algebraws()y)` to get the desired answer.\n\n\n\n"
},

{
    "location": "man/08-display.html#Reduce.Algebra.saveas",
    "page": "8 Display and Structuring of Expressions",
    "title": "Reduce.Algebra.saveas",
    "category": "function",
    "text": "saveas(expr)\n\nIf the user wishes to assign the workspace to a variable or expression for later use, the saveas statement can be used. It has the syntax\n\nR\"saveas ⟨expression⟩\"\n\nFor example, after the differentiation in the last example, the workspace holds the expression 2*x+2*y. If we wish to assign this to the variable z we can now say\n\njulia> Algebra.saveas(:z)\n\nIf the user wishes to save the expression in a form that allows him to use some of its variables as arbitrary parameters, the for all command can be used.\n\nExample:\n\nR\"for all x saveas h(x)\"\n\nwith the above expression would mean that h(z) evaluates to 2*y+2*z.\n\n\n\n"
},

{
    "location": "man/08-display.html#.2-The-Expression-Workspace-1",
    "page": "8 Display and Structuring of Expressions",
    "title": "8.2 The Expression Workspace",
    "category": "section",
    "text": "Reduce.Algebra.ws\nReduce.Algebra.saveasA further method for referencing more than the last expression is described in chapter 13 on interactive use of REDUCE."
},

{
    "location": "man/08-display.html#.3-Output-of-Expressions-1",
    "page": "8 Display and Structuring of Expressions",
    "title": "8.3 Output of Expressions",
    "category": "section",
    "text": "A considerable degree of flexibility is available in REDUCE in the printing of expressions generated during calculations. No explicit format statements are supplied, as these are in most cases of little use in algebraic calculations, where the size of output or its composition is not generally known in advance. Instead, REDUCE provides a series of mode options to the user that should enable him to produce his output in a comprehensible and possibly pleasing form.The most extreme option offered is to suppress the output entirely from any top level evaluation. This is accomplished by turning off the switch output which is normally on. It is useful for limiting output when loading large files or producing “clean” output from the prettyprint programs.In most circumstances, however, we wish to view the output, so we need to know how to format it appropriately. As we mentioned earlier, an algebraic expression is normally printed in an expanded form, filling the whole output line with terms. Certain output declarations, however, can be used to affect this format. To begin with, we look at an operator for changing the length of the output line."
},

{
    "location": "man/08-display.html#Reduce.linelength",
    "page": "8 Display and Structuring of Expressions",
    "title": "Reduce.linelength",
    "category": "function",
    "text": "linelength()\n\nThis operator is used with the syntax\n\nReduce.linelength()::Integer\n\nand sets the output line length to the integer tput cols. It returns the output line length (so that it can be stored for later resetting of the output line if needed).\n\n\n\n"
},

{
    "location": "man/08-display.html#.3.1-LINELENGTH-Operator-1",
    "page": "8 Display and Structuring of Expressions",
    "title": "8.3.1 LINELENGTH Operator",
    "category": "section",
    "text": "Reduce.linelength"
},

{
    "location": "man/08-display.html#.3.2-Output-Declarations-1",
    "page": "8 Display and Structuring of Expressions",
    "title": "8.3.2 Output Declarations",
    "category": "section",
    "text": "We now describe a number of switches and declarations that are available for controlling output formats. It should be noted, however, that the transformation of large expressions to produce these varied output formats can take a lot of computing time and space. If a user wishes to speed up the printing of the output in such cases, he can turn off the switch pri. If this is done, then output is produced in one fixed format, which basically reflects the internal form of the expression, and none of the options below apply. pri is normally on.With pri on, the output declarations and switches available are as follows:"
},

{
    "location": "man/08-display.html#Reduce.Algebra.order",
    "page": "8 Display and Structuring of Expressions",
    "title": "Reduce.Algebra.order",
    "category": "function",
    "text": "order(r...)\n\nThe declaration order may be used to order variables on output. The syntax is:\n\njulia> Algebra.order(v1,...vn)\n\nwhere the vi are kernels. Thus,\n\njulia> Algebra.order(:x,:y,:z)\n\norders x ahead of y, y ahead of z and all three ahead of other variables not given an order. order(nothing) resets the output order to the system default. The order of variables may be changed by further calls of order, but then the reordered variables would have an order lower than those in earlier order calls. Thus,\n\njulia> Algebra.order(:x,:y,:z)  \n\njulia> Algebra.order(:y,:x)\n\nwould order z ahead of y and x. The default ordering is usually alphabetic.\n\n\n\n"
},

{
    "location": "man/08-display.html#ORDER-Declaration-1",
    "page": "8 Display and Structuring of Expressions",
    "title": "ORDER Declaration",
    "category": "section",
    "text": "Reduce.Algebra.order"
},

{
    "location": "man/08-display.html#Reduce.factor",
    "page": "8 Display and Structuring of Expressions",
    "title": "Reduce.factor",
    "category": "function",
    "text": "factor(r...)\n\nThis declaration takes a list of identifiers or kernels as argument. factor is not a factoring command (use factorize or the factor switch for this purpose); rather it is a separation command. All terms involving fixed powers of the declared expressions are printed as a product of the fixed powers and a sum of the rest of the terms.\n\nFor example, after the declaration\n\njulia> Algebra.factor(:x)\n\nthe polynomial (x + y + 1)^2 will be printed as\n\n         2                  2  \n        x  + 2*x*(y + 1) + y  + 2*y + 1\n\nAll expressions involving a given prefix operator may also be factored by putting the operator name in the list of factored identifiers. For example:\n\njulia> Algebra.factor(:x,:cos,:(sin(x))\n\ncauses all powers of x and sin(x) and all functions of cos to be factored.\n\n\n\n"
},

{
    "location": "man/08-display.html#Reduce.Algebra.remfac",
    "page": "8 Display and Structuring of Expressions",
    "title": "Reduce.Algebra.remfac",
    "category": "function",
    "text": "remfac(r...)\n\nThe declaration remfac(v1,...,vn) removes the factoring flag from the expressions v1 through vn.\n\n\n\n"
},

{
    "location": "man/08-display.html#FACTOR-Declaration-1",
    "page": "8 Display and Structuring of Expressions",
    "title": "FACTOR Declaration",
    "category": "section",
    "text": "Reduce.Algebra.factorNote that factor does not affect the order of its arguments. You should also use order if this is important.Reduce.Algebra.remfac"
},

{
    "location": "man/08-display.html#.3.3-Output-Control-Switches-1",
    "page": "8 Display and Structuring of Expressions",
    "title": "8.3.3 Output Control Switches",
    "category": "section",
    "text": "In addition to these declarations, the form of the output can be modified by switching various output control switches using the declarations on and off. We shall illustrate the use of these switches by an example, namely the printing of the expressionR\"x^2*(y^2+2*y)+x*(y^2+z)/(2*a)\"The relevant switches are as follows:"
},

{
    "location": "man/08-display.html#ALLFAC-Switch-1",
    "page": "8 Display and Structuring of Expressions",
    "title": "ALLFAC Switch",
    "category": "section",
    "text": "This switch will cause the system to search the whole expression, or any sub-expression enclosed in parentheses, for simple multiplicative factors and print them outside the parentheses. Thus our expression with allfac off will print as    2  2        2          2  \n(2*x *y *a + 4*x *y*a + x*y  + x*z)/(2*a)and with allfac on as        2                2  \nx*(2*x*y *a + 4*x*y*a + y  + z)/(2*a)allfac is normally on, and is on in the following examples, except where otherwise stated."
},

{
    "location": "man/08-display.html#DIV-Switch-1",
    "page": "8 Display and Structuring of Expressions",
    "title": "DIV Switch",
    "category": "section",
    "text": "This switch makes the system search the denominator of an expression for simple factors that it divides into the numerator, so that rational fractions and negative powers appear in the output. With div on, our expression would print as      2                2  (-1)        (-1)  \nx*(x*y  + 2*x*y + 1/2*y *a     + 1/2*a    *z)div is normally off."
},

{
    "location": "man/08-display.html#LIST-Switch-1",
    "page": "8 Display and Structuring of Expressions",
    "title": "LIST Switch",
    "category": "section",
    "text": "This switch causes the system to print each term in any sum on a separate line. With list on, our expression prints as        2  \nx*(2*x*y *a  \n\n    + 4*x*y*a  \n\n       2  \n    + y  \n\n    + z)/(2*a)list is normally off."
},

{
    "location": "man/08-display.html#NOSPLIT-Switch-1",
    "page": "8 Display and Structuring of Expressions",
    "title": "NOSPLIT Switch",
    "category": "section",
    "text": "Under normal circumstances, the printing routines try to break an expression across lines at a natural point. This is a fairly expensive process. If you are not overly concerned about where the end-of-line breaks come, you can speed up the printing of expressions by turning off the switch nosplit. This switch is normally on."
},

{
    "location": "man/08-display.html#RAT-Switch-1",
    "page": "8 Display and Structuring of Expressions",
    "title": "RAT Switch",
    "category": "section",
    "text": "This switch is only useful with expressions in which variables are factored with factor. With this mode, the overall denominator of the expression is printed with each factored sub-expression. We assume a prior declaration factor(:x) in the following output. We first print the expression with rat set to off:    2                   2  \n(2*x *y*a*(y + 2) + x*(y  + z))/(2*a)With rat on the output becomes: 2                 2  \nx *y*(y + 2) + x*(y  + z)/(2*a)rat is normally off.Next, if we leave x factored, and turn on both div and rat, the result becomes 2                    (-1)   2  \nx *y*(y + 2) + 1/2*x*a    *(y  + z)Finally, with x factored, rat on and allfac off we retrieve the original structure 2   2              2  \nx *(y  + 2*y) + x*(y  + z)/(2*a)"
},

{
    "location": "man/08-display.html#RATPRI-Switch-1",
    "page": "8 Display and Structuring of Expressions",
    "title": "RATPRI Switch",
    "category": "section",
    "text": "If the numerator and denominator of an expression can each be printed in one line, the output routines will print them in a two dimensional notation, with numerator and denominator on separate lines and a line of dashes in between. For example, (a+b)/2 will print asA + B  \n-----  \n  2Turning this switch off causes such expressions to be output in a linear form."
},

{
    "location": "man/08-display.html#REVPRI-Switch-1",
    "page": "8 Display and Structuring of Expressions",
    "title": "REVPRI Switch",
    "category": "section",
    "text": "The normal ordering of terms in output is from highest to lowest power. In some situations (e.g., when a power series is output), the opposite ordering is more convenient. The switch revpri if on causes such a reverse ordering of terms. For example, the expression y*(x+1)^2+(y+3)^2 will normally print as 2              2  \nX *Y + 2*X*Y + Y  + 7*Y + 9whereas with revpri on, it will print as           2            2  \n9 + 7*Y + Y  + 2*X*Y + X *Y"
},

{
    "location": "man/08-display.html#.3.4-WRITE-Command-1",
    "page": "8 Display and Structuring of Expressions",
    "title": "8.3.4 WRITE Command",
    "category": "section",
    "text": "In simple cases no explicit output command is necessary in REDUCE, since the value of any expression is automatically printed if a semicolon is used as a delimiter. There are, however, several situations in which such a command is useful.In a for, while, or repeat statement it may be desired to output something each time the statement within the loop construct is repeated.It may be desired for a procedure to output intermediate results or other information while it is running. It may be desired to have results labeled in special ways, especially if the output is directed to a file or device other than the terminal.The write command consists of the word write followed by one or more items separated by commas, and followed by a terminator. There are three kinds of items that can be used:Expressions (including variables and constants). The expression is evaluated, and the result is printed out.\nAssignments. The expression on the right side of the := operator is evaluated, and is assigned to the variable on the left; then the symbol on the left is printed, followed by a “:=”, followed by the value of the expression on the right – almost exactly the way an assignment followed by a semicolon prints out normally. (The difference is that if the write is in a for statement and the left-hand side of the assignment is an array position or something similar containing the variable of the for iteration, then the value of that variable is inserted in the printout.)\nArbitrary strings of characters, preceded and followed by double-quote marks (e.g., R\"~string~\").The items specified by a single write statement print side by side on one line. (The line is broken automatically if it is too long.) Strings print exactly as quoted. The write command itself however does not return a value.The print line is closed at the end of a write command evaluation. Therefore the command R\"write ~~\" (specifying nothing to be printed except the empty string) causes a line to be skipped.Examples:If a is x+5, b is itself, c is 123, m is an array, and q=3, thenR\"write m(q):=a,~ ~,b/c,~ THANK YOU~\"will set m(3) to x+5 and printm(q) := x + 5 b/123 THANK YOUThe blanks between the 5 and b, and the 3 and t, come from the blanks in the quoted strings.To print a table of the squares of the integers from 1 to 20:R\"for i:=1:20 do write i,~ ~,i^2\"To print a table of the squares of the integers from 1 to 20, and at the same time store them in positions 1 to 20 of an array a:R\"for i:=1:20 do <<a(i):=i^2; write i,~ ~,a(i)>>\"This will give us two columns of numbers. If we had usedR\"for i:=1:20 do write i,~ ~,a(i):=i^2\"we would also get a(i) := repeated on each line.The following more complete example calculates the famous f and g series, first reported in Sconzo, P., LeSchack, A. R., and Tobey, R., “Symbolic Computation of f and g Series by Computer”, Astronomical Journal 70 (May 1965). x1:= -sig*(mu+2*eps)$  \n x2:= eps - 2*sig^2$  \n x3:= -3*mu*sig$  \n f:= 1$  \n g:= 0$  \n for i:= 1 step 1 until 10 do begin  \n    f1:= -mu*g+x1*df(f,eps)+x2*df(f,sig)+x3*df(f,mu);  \n    write ~f(~,i,~) := ~,f1;  \n    g1:= f+x1*df(g,eps)+x2*df(g,sig)+x3*df(g,mu);  \n    write ~g(~,i,~) := ~,g1;  \n    f:=f1$  \n    g:=g1$  \n   end;A portion of the output, to illustrate the printout from the write command, is as follows:                ... <prior output> ...  \n \n                           2  \n F(4) := MU*(3*EPS - 15*SIG  + MU)  \n \n G(4) := 6*SIG*MU  \n \n                                    2  \n F(5) := 15*SIG*MU*( - 3*EPS + 7*SIG  - MU)  \n \n                           2  \n G(5) := MU*(9*EPS - 45*SIG  + MU)  \n \n                ... <more output> ...  "
},

{
    "location": "man/08-display.html#.3.5-Suppression-of-Zeros-1",
    "page": "8 Display and Structuring of Expressions",
    "title": "8.3.5 Suppression of Zeros",
    "category": "section",
    "text": "It is sometimes annoying to have zero assignments (i.e. assignments of the form <expression> := 0) printed, especially in printing large arrays with many zero elements. The output from such assignments can be suppressed by turning on the switch nero."
},

{
    "location": "man/08-display.html#.3.6-FORTRAN-Style-Output-Of-Expressions-1",
    "page": "8 Display and Structuring of Expressions",
    "title": "8.3.6 FORTRAN Style Output Of Expressions",
    "category": "section",
    "text": "It is naturally possible to evaluate expressions numerically in REDUCE by giving all variables and sub-expressions numerical values. However, as we pointed out elsewhere the user must declare real arithmetical operation by turning on the switch rounded. However, it should be remembered that arithmetic in REDUCE is not particularly fast, since results are interpreted rather than evaluated in a compiled form. The user with a large amount of numerical computation after all necessary algebraic manipulations have been performed is therefore well advised to perform these calculations in a FORTRAN or similar system. For this purpose, REDUCE offers facilities for users to produce FORTRAN compatible files for numerical processing.First, when the switch fort is on, the system will print expressions in a FORTRAN notation. Expressions begin in column seven. If an expression extends over one line, a continuation mark (.) followed by a blank appears on subsequent cards. After a certain number of lines have been produced (according to the value of the variable card_no), a new expression is started. If the expression printed arises from an assignment to a variable, the variable is printed as the name of the expression. Otherwise the expression is given the default name ans. An error occurs if identifiers or numbers are outside the bounds permitted by FORTRAN.A second option is to use the write command to produce other programs.Example: The following REDUCE statements on fort;  \n out ~forfil~;  \n write ~C     this is a fortran program~;  \n write ~ 1    format(e13.5)~;  \n write ~      u=1.23~;  \n write ~      v=2.17~;  \n write ~      w=5.2~;  \n x:=(u+v+w)^11;  \n write ~C     it was foolish to expand this expression~;  \n write ~      print 1,x~;  \n write ~      end~;  \n shut ~forfil~;  \n off fort;will generate a file forfil that contains:c this is a fortran program  \n 1    format(e13.5)  \n      u=1.23  \n      v=2.17  \n      w=5.2  \n      ans1=1320.*u**3*v*w**7+165.*u**3*w**8+55.*u**2*v**9+495.*u  \n     . **2*v**8*w+1980.*u**2*v**7*w**2+4620.*u**2*v**6*w**3+  \n     . 6930.*u**2*v**5*w**4+6930.*u**2*v**4*w**5+4620.*u**2*v**3*  \n     . w**6+1980.*u**2*v**2*w**7+495.*u**2*v*w**8+55.*u**2*w**9+  \n     . 11.*u*v**10+110.*u*v**9*w+495.*u*v**8*w**2+1320.*u*v**7*w  \n     . **3+2310.*u*v**6*w**4+2772.*u*v**5*w**5+2310.*u*v**4*w**6  \n     . +1320.*u*v**3*w**7+495.*u*v**2*w**8+110.*u*v*w**9+11.*u*w  \n     . **10+v**11+11.*v**10*w+55.*v**9*w**2+165.*v**8*w**3+330.*  \n     . v**7*w**4+462.*v**6*w**5+462.*v**5*w**6+330.*v**4*w**7+  \n     . 165.*v**3*w**8+55.*v**2*w**9+11.*v*w**10+w**11  \n      x=u**11+11.*u**10*v+11.*u**10*w+55.*u**9*v**2+110.*u**9*v*  \n     . w+55.*u**9*w**2+165.*u**8*v**3+495.*u**8*v**2*w+495.*u**8  \n     . *v*w**2+165.*u**8*w**3+330.*u**7*v**4+1320.*u**7*v**3*w+  \n     . 1980.*u**7*v**2*w**2+1320.*u**7*v*w**3+330.*u**7*w**4+462.  \n     . *u**6*v**5+2310.*u**6*v**4*w+4620.*u**6*v**3*w**2+4620.*u  \n     . **6*v**2*w**3+2310.*u**6*v*w**4+462.*u**6*w**5+462.*u**5*  \n     . v**6+2772.*u**5*v**5*w+6930.*u**5*v**4*w**2+9240.*u**5*v  \n     . **3*w**3+6930.*u**5*v**2*w**4+2772.*u**5*v*w**5+462.*u**5  \n     . *w**6+330.*u**4*v**7+2310.*u**4*v**6*w+6930.*u**4*v**5*w  \n     . **2+11550.*u**4*v**4*w**3+11550.*u**4*v**3*w**4+6930.*u**  \n     . 4*v**2*w**5+2310.*u**4*v*w**6+330.*u**4*w**7+165.*u**3*v  \n     . **8+1320.*u**3*v**7*w+4620.*u**3*v**6*w**2+9240.*u**3*v**  \n     . 5*w**3+11550.*u**3*v**4*w**4+9240.*u**3*v**3*w**5+4620.*u  \n     . **3*v**2*w**6+ans1  \nc     it was foolish to expand this expression  \n      print 1,x  \n      endIf the arguments of a write statement include an expression that requires continuation records, the output will need editing, since the output routine prints the arguments of write sequentially, and the continuation mechanism therefore generates its auxiliary variables after the preceding expression has been printed.Finally, since there is no direct analog of list in FORTRAN, a comment line of the formc ***** invalid fortran construct (list) not printedwill be printed if you try to print a list with fort on."
},

{
    "location": "man/08-display.html#FORTRAN-Output-Options-1",
    "page": "8 Display and Structuring of Expressions",
    "title": "FORTRAN Output Options",
    "category": "section",
    "text": "There are a number of methods available to change the default format of the FORTRAN output.The breakup of the expression into subparts is such that the number of continuation lines produced is less than a given number. This number can be modified by the assignmentR\"card_no := ⟨number⟩\"where ⟨number⟩ is the total number of cards allowed in a statement. The default value of card_no is 20.The width of the output expression is also adjustable by the assignmentR\"fort_width := ⟨integer⟩\"fort_width which sets the total width of a given line to ⟨integer⟩. The initial FORTRAN output width is 70.REDUCE automatically inserts a decimal point after each isolated integer coefficient in a FORTRAN expression (so that, for example, 4 becomes 4.). To prevent this, set the period mode switch to off.FORTRAN output is normally produced in lower case. If upper case is desired, the switch FORTUPPER should be turned on.Finally, the default name ans assigned to an unnamed expression and its subparts can be changed by the operator varname. This takes a single identifier as argument, which then replaces ANS as the expression name. The value of varname is its argument.Further facilities for the production of FORTRAN and other language output are provided by the SCOPE and GENTRAN packagesdescribed in chapters 16.26 and 16.60."
},

{
    "location": "man/08-display.html#.3.7-Saving-Expressions-for-Later-Use-as-Input-1",
    "page": "8 Display and Structuring of Expressions",
    "title": "8.3.7 Saving Expressions for Later Use as Input",
    "category": "section",
    "text": "It is often useful to save an expression on an external file for use later as input in further calculations. The commands for opening and closing output files are explained elsewhere. However, we see in the examples on output of expressions that the standard “natural” method of printing expressions is not compatible with the input syntax. So to print the expression in an input compatible form we must inhibit this natural style by turning off the switch nat. If this is done, a dollar sign will also be printed at the end of the expression.Example: The following sequence of commands        off nat; out ~out~; x := (y+z)^2; write ~end~;  \n        shut ~out~; on nat;will generate a file out that contains        X := Y**2 + 2*Y*Z + Z**2$  \n        END$"
},

{
    "location": "man/08-display.html#.3.8-Displaying-Expression-Structure-1",
    "page": "8 Display and Structuring of Expressions",
    "title": "8.3.8 Displaying Expression Structure",
    "category": "section",
    "text": "In those cases where the final result has a complicated form, it is often convenient to display the skeletal structure of the answer. The operator structr, that takes a single expression as argument, will do this for you. Its syntax is:R\"structr(EXPRN:algebraic[,ID1:identifier[,ID2:identifier]])\"The structure is printed effectively as a tree, in which the subparts are laid out with auxiliary names. If the optional ID1 is absent, the auxiliary names are prefixed by the root ans. This root may be changed by the operator varname. If the optional ID1 is present, and is an array name, the subparts are named as elements of that array, otherwise ID1 is used as the root prefix. (The second optional argument ID2 is explained later.)The EXPRN can be either a scalar or a matrix expression. Use of any other will result in an error.Example: Let us suppose that the workspace contains ((a+b)^2+c)^3+d. Then the input R\"structr ws\" will (withexp` off) result in the output:ans3\n\n   where\n\n                  3\n      ans3 := ans2  + d\n\n                  2\n      ans2 := ans1  + c\n\n      ans1 := a + bThe workspace remains unchanged after this operation, since structr in the default situation returns no value (if structr is used as a sub-expression, its value is taken to be 0). In addition, the sub-expressions are normally only displayed and not retained. If you wish to access the sub-expressions with their displayed names, the switch savestructr should be turned on. In this case, structr returns a list whose first element is a representation for the expression, and subsequent elements are the sub-expression relations. Thus, with savestructr on, R\"structr ws\" in the above example would return               3              2\n{ans3,ans3=ans2  + d,ans2=ans1  + c,ans1=a + b}The part operator can be used to retrieve the required parts of the expression. For example, to get the value of ans2 in the above, one could say:R\"part(ws,3,2)\"If fort is on, then the results are printed in the reverse order; the algorithm in fact guaranteeing that no sub-expression will be referenced before it is defined. The second optional argument ID2 may also be used in this case to name the actual expression (or expressions in the case of a matrix argument).Example: Let us suppose that m, a 2 by 1 matrix, contains the elements ((a+b)^2 + c)^3 + d and (a + b)\\*(c + d) respectively, and that V has been declared to be an array. With exp off and fort on, the statement R\"structr(2\\*m,v,k);\" will result in the outputv(1)=a+b  \nv(2)=v(1)**2+c  \nv(3)=v(2)**3+d  \nv(4)=c+d  \nk(1,1)=2.*v(3)  \nk(2,1)=2.*v(1)*v(4)"
},

{
    "location": "man/08-display.html#Reduce.Algebra.korder",
    "page": "8 Display and Structuring of Expressions",
    "title": "Reduce.Algebra.korder",
    "category": "function",
    "text": "korder(r...)\n\nThe internal ordering of variables (more specifically kernels) can have a significant effect on the space and time associated with a calculation. In its default state, REDUCE uses a specific order for this which may vary between sessions. However, it is possible for the user to change this internal order by means of the declaration korder. The syntax for this is:\n\njulia> Algebra.korder(v1,...,vn)\n\nwhere the vi are kernels. With this declaration, the vi are ordered internally ahead of any other kernels in the system. v1 has the highest order, v2 the next highest, and so on. A further call of korder replaces a previous one. korder(nothing) resets the internal order to the system default.\n\n\n\n"
},

{
    "location": "man/08-display.html#.4-Changing-the-Internal-Order-of-Variables-1",
    "page": "8 Display and Structuring of Expressions",
    "title": "8.4 Changing the Internal Order of Variables",
    "category": "section",
    "text": "Reduce.Algebra.korderUnlike the order declaration, that has a purely cosmetic effect on the way results are printed, the use of korder can have a significant effect on computation time. In critical cases then, the user can experiment with the ordering of the variables used to determine the optimum set for a given problem."
},

{
    "location": "man/08-display.html#.5-Obtaining-Parts-of-Algebraic-Expressions-1",
    "page": "8 Display and Structuring of Expressions",
    "title": "8.5 Obtaining Parts of Algebraic Expressions",
    "category": "section",
    "text": "There are many occasions where it is desirable to obtain a specific part of an expression, or even change such a part to another expression. A number of operators are available in REDUCE for this purpose, and will be described in this section. In addition, operators for obtaining specific parts of polynomials and rational functions (such as a denominator) are described in another section."
},

{
    "location": "man/08-display.html#.5.1-COEFF-Operator-1",
    "page": "8 Display and Structuring of Expressions",
    "title": "8.5.1 COEFF Operator",
    "category": "section",
    "text": "Syntax:R\"coeff(EXPRN:polynomial,VAR:kernel)\"coeff is an operator that partitions EXPRN into its various coefficients with respect to VAR and returns them as a list, with the coefficient independent of VAR first.Under normal circumstances, an error results if EXPRN is not a polynomial in VAR, although the coefficients themselves can be rational as long as they do not depend on VAR. However, if the switch ratarg is on, denominators are not checked for dependence on VAR, and are taken to be part of the coefficients.Example:reduce> coeff((y^2+z)^3/z,y);returns the result  2  \n{Z ,0,3*Z,0,3,0,1/Z}whereasreduce> coeff((y^2+z)^3/y,y);gives an error if ratarg is off, and the result  3        2  \n{Z /Y,0,3*Z /Y,0,3*Z/Y,0,1/Y}if ratarg is on.The length of the result of coeff is the highest power of VAR encountered plus 1. In the above examples it is 7. In addition, the variable high_pow is set to the highest non-zero power found in EXPRN during the evaluation, and low_pow to the lowest non-zero power, or zero if there is a constant term. If EXPRN is a constant, then high_pow and low_pow are both set to zero."
},

{
    "location": "man/08-display.html#.5.2-COEFFN-Operator-1",
    "page": "8 Display and Structuring of Expressions",
    "title": "8.5.2 COEFFN Operator",
    "category": "section",
    "text": "The coeffn operator is designed to give the user a particular coefficient of a variable in a polynomial, as opposed to coeff that returns all coefficients. coeffn is used with the syntaxR\"coeffn(EXPRN:polynomial,VAR:kernel,N:integer)\"It returns the nth coefficient of VAR in the polynomial EXPRN."
},

{
    "location": "man/08-display.html#.5.3-PART-Operator-1",
    "page": "8 Display and Structuring of Expressions",
    "title": "8.5.3 PART Operator",
    "category": "section",
    "text": "Syntax:R\"part(EXPRN:algebraic[,INTEXP:integer])\"This operator works on the form of the expression as printed or as it would have been printed at that point in the calculation bearing in mind all the relevant switch settings at that point. The reader therefore needs some familiarity with the way that expressions are represented in prefix form in REDUCE to use these operators effectively. Furthermore, it is assumed that pri is on at that point in the calculation. The reason for this is that with pri off, an expression is printed by walking the tree representing the expression internally. To save space, it is never actually transformed into the equivalent prefix expression as occurs when pri is on. However, the operations on polynomials described elsewhere can be equally well used in this case to obtain the relevant parts.The evaluation proceeds recursively down the integer expression list. In other words,part(⟨expression⟩,⟨integer1⟩,⟨integer2⟩)\n→part(part(⟨expression⟩,⟨integer1⟩),⟨integer2⟩)and so on, andPART(⟨expression⟩)→⟨expression⟩intexp can be any expression that evaluates to an integer. If the integer is positive, then that term of the expression is found. If the integer is 0, the operator is returned. Finally, if the integer is negative, the counting is from the tail of the expression rather than the head.For example, if the expression a+b is printed as a+b (i.e., the ordering of the variables is alphabetical), then        part(a+b,2)  ->   b  \n        part(a+b,-1) ->   b  and        part(a+b,0)  ->  plusAn operator arglength is available to determine the number of arguments of the top level operator in an expression. If the expression does not contain a top level operator, then -1 is returned. For example,        arglength(a+b+c) ->  3  \n        arglength(f())   ->  0  \n        arglength(a)     ->  -1"
},

{
    "location": "man/08-display.html#.5.4-Substituting-for-Parts-of-Expressions-1",
    "page": "8 Display and Structuring of Expressions",
    "title": "8.5.4 Substituting for Parts of Expressions",
    "category": "section",
    "text": "part may also be used to substitute for a given part of an expression. In this case, the part construct appears on the left-hand side of an assignment statement, and the expression to replace the given part on the right-hand side.For example, with the normal settings of the REDUCE switches:        xx := a+b;  \n        part(xx,2) := c;   ->  A+C  \n        part(c+d,0) := -;   -> C-DNote that xx in the above is not changed by this substitution. In addition, unlike expressions such as array and matrix elements that have an instant evaluation property, the values of part(xx,2) and part(c+d,0) are also not changed."
},

{
    "location": "man/09-polynomials.html#",
    "page": "9 Polynomials and Rationals",
    "title": "9 Polynomials and Rationals",
    "category": "page",
    "text": ""
},

{
    "location": "man/09-polynomials.html#Polynomials-and-Rationals-1",
    "page": "9 Polynomials and Rationals",
    "title": "9 Polynomials and Rationals",
    "category": "section",
    "text": "Many operations in computer algebra are concerned with polynomials and rational functions. In this section, we review some of the switches and operators available for this purpose. These are in addition to those that work on general expressions (such as df and int) described elsewhere. In the case of operators, the arguments are first simplified before the operations are applied. In addition, they operate only on arguments of prescribed types, and produce a type mismatch error if given arguments which cannot be interpreted in the required mode with the current switch settings. For example, if an argument is required to be a kernel and a/2 is used (with no other rules for a), an error        A/2 invalid as kernelwill result.With the exception of those that select various parts of a polynomial or rational function, these operations have potentially significant effects on the space and time associated with a given calculation. The user should therefore experiment with their use in a given calculation in order to determine the optimum set for a given problem.One such operation provided by the system is an operator length which returns the number of top level terms in the numerator of its argument. For example,julia> Algebra.length(:((a+b+c)^3/(c+d)))has the value 10. To get the number of terms in the denominator, one would first select the denominator by the operator den and then call length, as injulia> Algebra.length(Algebra.den(:((a+b+c)^3/(c+d))))Other operations currently supported, the relevant switches and operators, and the required argument and value modes of the latter, follow.Pages = [\"09-polynomials.md\"]"
},

{
    "location": "man/09-polynomials.html#.1-Controlling-the-Expansion-of-Expressions-1",
    "page": "9 Polynomials and Rationals",
    "title": "9.1 Controlling the Expansion of Expressions",
    "category": "section",
    "text": "The switch exp controls the expansion of expressions. If it is off, no expansion of powers or products of expressions occurs. Users should note however that in this case results come out in a normal but not necessarily canonical form. This means that zero expressions simplify to zero, but that two equivalent expressions need not necessarily simplify to the same form.Example: With exp on, the two expressions(a+b)*(a+2*b)anda^2+3*a*b+2*b^2will both simplify to the latter form. With exp off, they would remain unchanged, unless the complete factoring (allfac) option were in force. exp is normally on.Note that in Reduce.jl exp is turned off by default on initializationSeveral operators that expect a polynomial as an argument behave differently when exp is off, since there is often only one term at the top level. For example, with exp offjulia> Algebra.length(:((a+b+c)^3/(c+d)))returns the value 1."
},

{
    "location": "man/09-polynomials.html#Reduce.Algebra.factorize",
    "page": "9 Polynomials and Rationals",
    "title": "Reduce.Algebra.factorize",
    "category": "function",
    "text": "factorize(r...)\n\nIt is also possible to factorize a given expression explicitly. The operator factorize that invokes this facility is used with the syntax\n\nR\"factorize(EXPRN:polynomial[,INTEXP:prime integer])\"\n\nthe optional argument of which will be described later. Thus to find and display all factors of the cyclotomic polynomial x^105 - 1, one could write:\n\njulia> Algebra.factorize(:(x^105-1))\n\nThe result is a list of factor,exponent pairs. In the above example, there is no overall numerical factor in the result, so the results will consist only of polynomials in x. The number of such polynomials can be found by using the operator length. If there is a numerical factor, as in factorizing 12x^2 - 12, that factor will appear as the first member of the result. It will however not be factored further. Prime factors of such numbers can be found, using a probabilistic algorithm, by turning on the switch ifactor. For example,\n\njulia> Algebra.on(:ifactor); Algebra.factorize(:(12x^2-12))\n\nwould result in the output\n\n((2, 2), (3, 1), (:(x ^ 2 + 1), 1), (:(x + 1), 1), (:(x - 1), 1))\n\nIf the first argument of factorize is an integer, it will be decomposed into its prime components, whether or not ifactor is on.\n\nNote that the ifactor switch only affects the result of factorize. It has no effect if the factor switch is also on.\n\n\n\n"
},

{
    "location": "man/09-polynomials.html#.2-Factorization-of-Polynomials-1",
    "page": "9 Polynomials and Rationals",
    "title": "9.2 Factorization of Polynomials",
    "category": "section",
    "text": "REDUCE is capable of factorizing univariate and multivariate polynomials that have integer coefficients, finding all factors that also have integer coefficients. The package for doing this was written by Dr. Arthur C. Norman and Ms. P. Mary Ann Moore at The University of Cambridge. It is described in P. M. A. Moore and A. C. Norman, “Implementing a Polynomial Factorization and GCD Package”, Proc. SYMSAC ’81, ACM (New York) (1981), 109-116.The easiest way to use this facility is to turn on the switch factor, which causes all expressions to be output in a factored form. For example, with factor on, the expression a^2-b^2 is returned as (a+b)*(a-b).Reduce.Algebra.factorizeThe order in which the factors occur in the result (with the exception of a possible overall numerical coefficient which comes first) can be system dependent and should not be relied on. Similarly it should be noted that any pair of individual factors can be negated without altering their product, and that REDUCE may sometimes do that.The factorizer works by first reducing multivariate problems to univariate ones and then solving the univariate ones modulo small primes. It normally selects both evaluation points and primes using a random number generator that should lead to different detailed behavior each time any particular problem is tackled. If, for some reason, it is known that a certain (probably univariate) factorization can be performed effectively with a known prime, p say, this value of p can be handed to factorize as a second argument. An error will occur if a non-prime is provided to factorize in this manner. It is also an error to specify a prime that divides the discriminant of the polynomial being factored, but users should note that this condition is not checked by the program, so this capability should be used with care.Factorization can be performed over a number of polynomial coefficient domains in addition to integers. The particular description of the relevant domain should be consulted to see if factorization is supported. For example, the following statements will factorize x^4 + 1 modulo 7:        setmod 7;  \n        on modular;  \n        factorize(x^4+1);The factorization module is provided with a trace facility that may be useful as a way of monitoring progress on large problems, and of satisfying curiosity about the internal workings of the package. The most simple use of this is enabled by issuing the REDUCE command on(:trfac). Following this, all calls to the factorizer will generate informative messages reporting on such things as the reduction of multivariate to univariate cases, the choice of a prime and the reconstruction of full factors from their images. Further levels of detail in the trace are intended mainly for system tuners and for the investigation of suspected bugs. For example, trallfac gives tracing information at all levels of detail. The switch that can be set by on(:timings) makes it possible for one who is familiar with the algorithms used to determine what part of the factorization code is consuming the most resources. on(:overview) reduces the amount of detail presented in other forms of trace. Other forms of trace output are enabled by directives of the form        symbolic set!-trace!-factor(<number>,<filename>);where useful numbers are 1, 2, 3 and 100, 101, ... . This facility is intended to make it possible to discover in fairly great detail what just some small part of the code has been doing — the numbers refer mainly to depths of recursion when the factorizer calls itself, and to the split between its work forming and factorizing images and reconstructing full factors from these. If nil is used in place of a filename the trace output requested is directed to the standard output stream. After use of this trace facility the generated trace files should be closed by calling        symbolic close!-trace!-files();NOTE: Using the factorizer with mcd off will result in an error."
},

{
    "location": "man/09-polynomials.html#.3-Cancellation-of-Common-Factors-1",
    "page": "9 Polynomials and Rationals",
    "title": "9.3 Cancellation of Common Factors",
    "category": "section",
    "text": "Facilities are available in REDUCE for cancelling common factors in the numerators and denominators of expressions, at the option of the user. The system will perform this greatest common divisor computation if the switch gcd is on. (gcd is normally off.)A check is automatically made, however, for common variable and numerical products in the numerators and denominators of expressions, and the appropriate cancellations made.When gcd is on, and exp is off, a check is made for square free factors in an expression. This includes separating out and independently checking the content of a given polynomial where appropriate. (For an explanation of these terms, see Anthony C. Hearn, “Non-Modular Computation of Polynomial GCDs Using Trial Division”, Proc. EUROSAM 79, published as Lecture Notes on Comp. Science, Springer-Verlag, Berlin, No 72 (1979) 227-239.)Example: With exp off and gcd on, the polynomial a*c+a*d+b*c+b*d would be returned as (a+b)*(c+d).Under normal circumstances, GCDs are computed using an algorithm described in the above paper. It is also possible in REDUCE to compute GCDs using an alternative algorithm, called the EZGCD Algorithm, which uses modular arithmetic. The switch ezgcd, if on in addition to gcd, makes this happen.In non-trivial cases, the EZGCD algorithm is almost always better than the basic algorithm, often by orders of magnitude. We therefore strongly advise users to use the ezgcd switch where they have the resources available for supporting the package.For a description of the EZGCD algorithm, see J. Moses and D.Y.Y. Yun, “The EZ GCD Algorithm”, Proc. ACM 1973, ACM, New York (1973) 159-166.NOTE: This package shares code with the factorizer, so a certain amount of trace information can be produced using the factorizer trace switches.An implementation of the heuristic GCD algorithm, first introduced by B.W. Char, K.O. Geddes and G.H. Gonnet, as described in J.H. Davenport and J. Padget, “HEUGCD: How Elementary Upperbounds Generate Cheaper Data”, Proc. of EUROCAL ’85, Vol 2, 18-28, published as Lecture Notes on Comp. Science, No. 204, Springer-Verlag, Berlin, 1985, is also available on an experimental basis. To use this algorithm, the switch heugcd should be on in addition to gcd. Note that if both ezgcd and heugcd are on, the former takes precedence."
},

{
    "location": "man/09-polynomials.html#.3.1-Determining-the-GCD-of-Two-Polynomials-1",
    "page": "9 Polynomials and Rationals",
    "title": "9.3.1 Determining the GCD of Two Polynomials",
    "category": "section",
    "text": "This operator, used with the syntaxR\"gcd(EXPRN1:polynomial,EXPRN2:polynomial)\"returns the greatest common divisor of the two polynomials EXPRN1 and EXPRN2. Examples:        gcd(x^2+2*x+1,x^2+3*x+2) ->  x+1  \n        gcd(2*x^2-2*y^2,4*x+4*y) ->  2*x+2*y  \n        gcd(x^2+y^2,x-y)         ->  1."
},

{
    "location": "man/09-polynomials.html#.4-Working-with-Least-Common-Multiples-1",
    "page": "9 Polynomials and Rationals",
    "title": "9.4 Working with Least Common Multiples",
    "category": "section",
    "text": "Greatest common divisor calculations can often become expensive if extensive work with large rational expressions is required. However, in many cases, the only significant cancellations arise from the fact that there are often common factors in the various denominators which are combined when two rationals are added. Since these denominators tend to be smaller and more regular in structure than the numerators, considerable savings in both time and space can occur if a full GCD check is made when the denominators are combined and only a partial check when numerators are constructed. In other words, the true least common multiple of the denominators is computed at each step. The switch lcm is available for this purpose, and is normally on.In addition, the operator lcm, used with the syntaxR\"lcm(EXPRN1:polynomial,EXPRN2:polynomial)\"returns the least common multiple of the two polynomials EXPRN1 and EXPRN2.Examples:        lcm(x^2+2*x+1,x^2+3*x+2) ->  X**3 + 4*X**2 + 5*X + 2  \n        lcm(2*x^2-2*y^2,4*x+4*y) ->  4*(X**2 - Y**2)9.4 "
},

{
    "location": "man/09-polynomials.html#.5-Controlling-Use-of-Common-Denominators-1",
    "page": "9 Polynomials and Rationals",
    "title": "9.5 Controlling Use of Common Denominators",
    "category": "section",
    "text": "When two rational functions are added, REDUCE normally produces an expression over a common denominator. However, if the user does not want denominators combined, he or she can turn off the switch mcd which controls this process. The latter switch is particularly useful if no greatest common divisor calculations are desired, or excessive differentiation of rational functions is required.CAUTION: With mcd off, results are not guaranteed to come out in either normal or canonical form. In other words, an expression equivalent to zero may in fact not be simplified to zero. This option is therefore most useful for avoiding expression swell during intermediate parts of a calculation.mcd is normally on."
},

{
    "location": "man/09-polynomials.html#Reduce.Algebra.remainder",
    "page": "9 Polynomials and Rationals",
    "title": "Reduce.Algebra.remainder",
    "category": "function",
    "text": "remainder(a,b)\n\nThis operator is used with the syntax\n\nR\"REMAINDER(EXPRN1:polynomial,EXPRN2:polynomial)\"\n\nIt returns the remainder when EXPRN1 is divided by EXPRN2. This is the true remainder based on the internal ordering of the variables, and not the pseudo-remainder. The pseudo-remainder and in general pseudo-division of polynomials can be calculated after loading the polydiv package. Please refer to the documentation of this package for details.\n\nExamples:\n\n        remainder((x+y)*(x+2*y),x+3*y) ->  2*y**2  \n        remainder(2*x+y,2)             ->  Y\n\n\n\n"
},

{
    "location": "man/09-polynomials.html#.6-REMAINDER-Operator-1",
    "page": "9 Polynomials and Rationals",
    "title": "9.6 REMAINDER Operator",
    "category": "section",
    "text": "Reduce.Algebra.remainderCAUTION: In the default case, remainders are calculated over the integers. If you need the remainder with respect to another domain, it must be declared explicitly.Example:        remainder(x^2-2,x+sqrt(2)); -> x^2 - 2  \n        load_package arnum;  \n        defpoly sqrt2**2-2;  \n        remainder(x^2-2,x+sqrt2); -> 0"
},

{
    "location": "man/09-polynomials.html#Reduce.Algebra.resultant",
    "page": "9 Polynomials and Rationals",
    "title": "Reduce.Algebra.resultant",
    "category": "function",
    "text": "resultant(a,b,var)\n\nThis is used with the syntax\n\nR\"resultant(EXPRN1:polynomial,EXPRN2:polynomial,VAR:kernel)\"\n\nIt computes the resultant of the two given polynomials with respect to the given variable, the coefficients of the polynomials can be taken from any domain. The result can be identified as the determinant of a Sylvester matrix, but can often also be thought of informally as the result obtained when the given variable is eliminated between the two input polynomials. If the two input polynomials have a non-trivial GCD their resultant vanishes.\n\nThe switch bezout controls the computation of the resultants. It is off by default. In this case a subresultant algorithm is used. If the switch Bezout is turned on, the resultant is computed via the Bezout Matrix. However, in the latter case, only polynomial coefficients are permitted.\n\n\n\n"
},

{
    "location": "man/09-polynomials.html#.7-RESULTANT-Operator-1",
    "page": "9 Polynomials and Rationals",
    "title": "9.7 RESULTANT Operator",
    "category": "section",
    "text": "Reduce.Algebra.resultantThe sign conventions used by the resultant function follow those in R. Loos, “Computing in Algebraic Extensions” in “Computer Algebra — Symbolic and Algebraic Computation”, Second Ed., Edited by B. Buchberger, G.E. Collins and R. Loos, Springer-Verlag, 1983. Namely, with a and b not dependent on x:                               deg(p)*deg(q)  \n   resultant(p(x),q(x),x)= (-1)             *resultant(q,p,x)  \n \n                            deg(p)  \n   resultant(a,p(x),x)   = a  \n \n   resultant(a,b,x)      = 1Examples:                                     2  \n   resultant(x/r*u+y,u*y,u)   ->  - ycalculation in an algebraic extension:   load arnum;  \n   defpoly sqrt2**2 - 2;  \n \n   resultant(x + sqrt2,sqrt2 * x +1,x) -> -1or in a modular domain:   setmod 17;  \n   on modular;  \n \n   resultant(2x+1,3x+4,x)    -> 5"
},

{
    "location": "man/09-polynomials.html#Reduce.Algebra.decompose",
    "page": "9 Polynomials and Rationals",
    "title": "Reduce.Algebra.decompose",
    "category": "function",
    "text": "decompose(p)\n\nThe decompose operator takes a multivariate polynomial as argument, and returns an expression and a list of equations from which the original polynomial can be found by composition. Its syntax is:\n\nR\"decompose(EXPRN:polynomial)\"\n\nFor example:\n\n     decompose(x^8-88*x^7+2924*x^6-43912*x^5+263431*x^4-  \n                    218900*x^3+65690*x^2-7700*x+234)  \n                   2                  2            2  \n              -> {U  + 35*U + 234, U=V  + 10*V, V=X  - 22*X}  \n                                     2  \n     decompose(u^2+v^2+2u*v+1)  -> {W  + 1, W=U + V}\n\nUsers should note however that, unlike factorization, this decomposition is not unique.\n\n\n\n"
},

{
    "location": "man/09-polynomials.html#.8-DECOMPOSE-Operator-1",
    "page": "9 Polynomials and Rationals",
    "title": "9.8 DECOMPOSE Operator",
    "category": "section",
    "text": "Reduce.Algebra.decompose"
},

{
    "location": "man/09-polynomials.html#Reduce.Algebra.interpol",
    "page": "9 Polynomials and Rationals",
    "title": "Reduce.Algebra.interpol",
    "category": "function",
    "text": "interpol(val,var,mp)\n\nSyntax:\n\nR\"interpol(⟨values⟩,⟨variable⟩,metapoints)\"\n\nwhere ⟨values⟩ and ⟨points⟩ are lists of equal length and <variable> is an algebraic expression (preferably a kernel).\n\ninterpol generates an interpolation polynomial f in the given variable of degree length(⟨values⟩)-1. The unique polynomial f is defined by the property that for corresponding elements v of ⟨values⟩ and p of ⟨points⟩ the relation f(p) = v holds.\n\nThe Aitken-Neville interpolation algorithm is used which guarantees a stable result even with rounded numbers and an ill-conditioned problem.\n\n\n\n"
},

{
    "location": "man/09-polynomials.html#.9-INTERPOL-operator-1",
    "page": "9 Polynomials and Rationals",
    "title": "9.9 INTERPOL operator",
    "category": "section",
    "text": "Reduce.Algebra.interpol"
},

{
    "location": "man/09-polynomials.html#Reduce.Algebra.deg",
    "page": "9 Polynomials and Rationals",
    "title": "Reduce.Algebra.deg",
    "category": "function",
    "text": "deg(p,var)\n\nThis operator is used with the syntax\n\nR\"deg(EXPRN:polynomial,VAR:kernel)\"\n\nIt returns the leading degree of the polynomial EXPRN in the variable VAR. If VAR does not occur as a variable in EXPRN, 0 is returned.\n\nExamples:\n\n        deg((a+b)*(c+2*d)^2,a) ->  1  \n        deg((a+b)*(c+2*d)^2,d) ->  2  \n        deg((a+b)*(c+2*d)^2,e) ->  0\n\nNote also that if ratarg is on,\n\n        deg((a+b)^3/a,a)       ->  3\n\nsince in this case, the denominator a is considered part of the coefficients of the numerator in a. With ratarg off, however, an error would result in this case.\n\n\n\n"
},

{
    "location": "man/09-polynomials.html#Reduce.Algebra.den",
    "page": "9 Polynomials and Rationals",
    "title": "Reduce.Algebra.den",
    "category": "function",
    "text": "den(r)\n\nThis is used with the syntax:\n\nR\"den(EXPRN:rational)\"\n\nIt returns the denominator of the rational expression EXPRN. If EXPRN is a polynomial, 1 is returned.\n\nExamples:\n\n        den(x/y^2)   ->  Y**2  \n        den(100/6)   ->  3  \n                [since 100/6 is first simplified to 50/3]  \n        den(a/4+b/6) ->  12  \n        den(a+b)     ->  1\n\n\n\n"
},

{
    "location": "man/09-polynomials.html#Reduce.Algebra.lcof",
    "page": "9 Polynomials and Rationals",
    "title": "Reduce.Algebra.lcof",
    "category": "function",
    "text": "lcof(expr,var)\n\nlcof is used with the syntax\n\nR\"lcof(EXPRN:polynomial,VAR:kernel)\"\n\nIt returns the leading coefficient of the polynomial EXPRN in the variable VAR. If VAR does not occur as a variable in EXPRN, EXPRN is returned.\n\nExamples:\n\n        lcof((a+b)*(c+2*d)^2,a) ->  c**2+4*c*d+4*d**2  \n        lcof((a+b)*(c+2*d)^2,d) ->  4*(a+b)  \n        lcof((a+b)*(c+2*d),e)   ->  a*c+2*a*d+b*c+2*b*d\n\n\n\n"
},

{
    "location": "man/09-polynomials.html#Reduce.Algebra.lpower",
    "page": "9 Polynomials and Rationals",
    "title": "Reduce.Algebra.lpower",
    "category": "function",
    "text": "lpower(exprn,var)\n\nSyntax:\n\nR\"lpower(EXPRN:polynomial,VAR:kernel)\"\n\nlpower returns the leading power of EXPRN with respect to VAR. If EXPRN does not depend on VAR, 1 is returned. Examples:\n\n        lpower((a+b)*(c+2*d)^2,a) ->  a  \n        lpower((a+b)*(c+2*d)^2,d) ->  d**2  \n        lpower((a+b)*(c+2*d),e)   ->  1\n\n\n\n"
},

{
    "location": "man/09-polynomials.html#Reduce.Algebra.lterm",
    "page": "9 Polynomials and Rationals",
    "title": "Reduce.Algebra.lterm",
    "category": "function",
    "text": "lterm(exprn,var)\n\nSyntax:\n\nR\"lterm(EXPRN:polynomial,VAR:kernel)\"\n\nlterm returns the leading term of EXPRN with respect to VAR. If EXPRN does not depend on VAR, EXPRN is returned.\n\nExamples:\n\n        lterm((a+b)*(c+2*d)^2,a) ->  a*(c**2+4*c*d+4*d**2)  \n        lterm((a+b)*(c+2*d)^2,d) ->  4*d**2*(a+b)  \n        lterm((a+b)*(c+2*d),e)   ->  a*c+2*a*d+b*c+2*b*d\n\n\n\n"
},

{
    "location": "man/09-polynomials.html#Reduce.Algebra.mainvar",
    "page": "9 Polynomials and Rationals",
    "title": "Reduce.Algebra.mainvar",
    "category": "function",
    "text": "mainvar(exprn)\n\nSyntax:\n\nR\"mainvar(EXPRN:polynomial)\"\n\nReturns the main variable (based on the internal polynomial representation) of EXPRN. If EXPRN is a domain element, 0 is returned.\n\nExamples: Assuming a has higher kernel order than b, c, or d:\n\n        mainvar((a+b)*(c+2*d)^2) ->  a\n        mainvar(2)               ->  0\n\n\n\n"
},

{
    "location": "man/09-polynomials.html#Reduce.Algebra.num",
    "page": "9 Polynomials and Rationals",
    "title": "Reduce.Algebra.num",
    "category": "function",
    "text": "num(exprn)\n\nSyntax:\n\nR\"num(EXPRN:rational)\"\n\nReturns the numerator of the rational expression EXPRN. If EXPRN is a polynomial, that polynomial is returned.\n\nExamples:\n\n        num(x/y^2)  ->  x  \n        num(100/6)   ->  50  \n        num(a/4+b/6) ->  3*a+2*b  \n        num(a+b)     ->  a+b\n\n\n\n"
},

{
    "location": "man/09-polynomials.html#Reduce.Algebra.reduct",
    "page": "9 Polynomials and Rationals",
    "title": "Reduce.Algebra.reduct",
    "category": "function",
    "text": "reduct(exprn,var)\n\nSyntax:\n\nR\"reduct(EXPRN:polynomial,VAR:kernel)\"\n\nReturns the reductum of EXPRN with respect to VAR (i.e., the part of EXPRN left after the leading term is removed). If EXPRN does not depend on the variable VAR, 0 is returned.\n\nExamples:\n\n     reduct((a+b)*(c+2*d),a) ->  b*(c + 2*d)  \n     reduct((a+b)*(c+2*d),d) ->  c*(a + b)  \n     reduct((a+b)*(c+2*d),e) ->  0\n\n\n\n"
},

{
    "location": "man/09-polynomials.html#Reduce.Algebra.totaldeg",
    "page": "9 Polynomials and Rationals",
    "title": "Reduce.Algebra.totaldeg",
    "category": "function",
    "text": "totaldeg(expr,var)\n\nSyntax:\n\n     totaldeg(a*x^2+b*x+c, x)  => 2  \n     totaldeg(a*x^2+b*x+c, {a,b,c})  => 1  \n     totaldeg(a*x^2+b*x+c, {x, a})  => 3  \n     totaldeg(a*x^2+b*x+c, {x,b})  => 2  \n     totaldeg(a*x^2+b*x+c, {p,q,r})  => 0\n\ntotaldeg(u, kernlist) finds the total degree of the polynomial u in the variables in kernlist. If kernlist is not a list it is treated as a simple single variable. The denominator of u is ignored, and \"degree\" here does not pay attention to fractional powers. Mentions of a kernel within the argument to any operator or function (eg sin, cos, log, sqrt) are ignored. Really u is expected to be just a polynomial.\n\n\n\n"
},

{
    "location": "man/09-polynomials.html#.10-Obtaining-Parts-of-Polynomials-and-Rationals-1",
    "page": "9 Polynomials and Rationals",
    "title": "9.10 Obtaining Parts of Polynomials and Rationals",
    "category": "section",
    "text": "These operators select various parts of a polynomial or rational function structure. Except for the cost of rearrangement of the structure, these operations take very little time to perform.For those operators in this section that take a kernel VAR as their second argument, an error results if the first expression is not a polynomial in VAR, although the coefficients themselves can be rational as long as they do not depend on VAR. However, if the switch ratarg is on, denominators are not checked for dependence on VAR, and are taken to be part of the coefficients.Reduce.Algebra.deg\nReduce.Algebra.den\nReduce.Algebra.lcof\nReduce.Algebra.lpower\nReduce.Algebra.ltermCompatibility Note:  In some earlier versions of REDUCE, lterm returned 0 if the EXPRN did not depend on VAR. In the present version, EXPRN is always equal to lterm(EXPRN,VAR) + reduct(EXPRN,VAR).Reduce.Algebra.mainvar\nReduce.Algebra.num\nReduce.Algebra.reductCompatibility Note:  In some earlier versions of REDUCE, reduct returned EXPRN if it did not depend on VAR. In the present version, EXPRN is always equal to lterm(EXPRN,VAR) + reduct(EXPRN,VAR).Reduce.Algebra.totaldeg"
},

{
    "location": "man/09-polynomials.html#.11-Polynomial-Coefficient-Arithmetic-1",
    "page": "9 Polynomials and Rationals",
    "title": "9.11 Polynomial Coefficient Arithmetic",
    "category": "section",
    "text": "REDUCE allows for a variety of numerical domains for the numerical coefficients of polynomials used in calculations. The default mode is integer arithmetic, although the possibility of using real coefficients has been discussed elsewhere. Rational coefficients have also been available by using integer coefficients in both the numerator and denominator of an expression, using the on(:div) option to print the coefficients as rationals. However, REDUCE includes several other coefficient options in its basic version which we shall describe in this section. All such coefficient modes are supported in a table-driven manner so that it is straightforward to extend the range of possibilities. A description of how to do this is given in R.J. Bradford, A.C. Hearn, J.A. Padget and E. Schrüfer, “Enlarging the REDUCE Domain of Computation,” Proc. of SYMSAC ’86, ACM, New York (1986), 100–106."
},

{
    "location": "man/09-polynomials.html#.11.1-Rational-Coefficients-in-Polynomials-1",
    "page": "9 Polynomials and Rationals",
    "title": "9.11.1 Rational Coefficients in Polynomials",
    "category": "section",
    "text": "Instead of treating rational numbers as the numerator and denominator of a rational expression, it is also possible to use them as polynomial coefficients directly. This is accomplished by turning on the switch rational.Example: With rational off, the input expression a/2 would be converted into a rational expression, whose numerator was a and denominator 2. With rational on, the same input would become a rational expression with numerator 1/2*a and denominator 1. Thus the latter can be used in operations that require polynomial input whereas the former could not."
},

{
    "location": "man/09-polynomials.html#.11.2-Real-Coefficients-in-Polynomials-1",
    "page": "9 Polynomials and Rationals",
    "title": "9.11.2 Real Coefficients in Polynomials",
    "category": "section",
    "text": "The switch rounded permits the use of arbitrary sized real coefficients in polynomial expressions. The actual precision of these coefficients can be set by the operator precision. For example, precision(50) sets the precision to fifty decimal digits. The default precision is system dependent and can be found by precision(0). In this mode, denominators are automatically made monic, and an appropriate adjustment is made to the numerator.Example: With rounded on, the input expression a/2 would be converted into a rational expression whose numerator is 0.5*a and denominator 1.Internally, REDUCE uses floating point numbers up to the precision supported by the underlying machine hardware, and so-called bigfloats for higher precision or whenever necessary to represent numbers whose value cannot be represented in floating point. The internal precision is two decimal digits greater than the external precision to guard against roundoff inaccuracies. Bigfloats represent the fraction and exponent parts of a floating-point number by means of (arbitrary precision) integers, which is a more precise representation in many cases than the machine floating point arithmetic, but not as efficient. If a case arises where use of the machine arithmetic leads to problems, a user can force REDUCE to use the bigfloat representation at all precisions by turning on the switch roundbf. In rare cases, this switch is turned on by the system, and the user informed by the message        ROUNDBF turned on to increase accuracyRounded numbers are normally printed to the specified precision. However, if the user wishes to print such numbers with less precision, the printing precision can be set by the command print_precision. For example, print_precision(5) will cause such numbers to be printed with five digits maximum.Under normal circumstances when rounded is on, REDUCE converts the number 1.0 to the integer 1. If this is not desired, the switch noconvert can be turned on.Numbers that are stored internally as bigfloats are normally printed with a space between every five digits to improve readability. If this feature is not required, it can be suppressed by turning off the switch bfspace.Further information on the bigfloat arithmetic may be found in T. Sasaki, “Manual for Arbitrary Precision Real Arithmetic System in REDUCE”, Department of Computer Science, University of Utah, Technical Note No. TR-8 (1979).When a real number is input, it is normally truncated to the precision in effect at the time the number is read. If it is desired to keep the full precision of all numbers input, the switch adjprec (for adjust precision) can be turned on. While on, adjprec will automatically increase the precision, when necessary, to match that of any integer or real input, and a message printed to inform the user of the precision increase.When rounded is on, rational numbers are normally converted to rounded representation. However, if a user wishes to keep such numbers in a rational form until used in an operation that returns a real number, the switch roundall can be turned off. This switch is normally on.Results from rounded calculations are returned in rounded form with two exceptions: if the result is recognized as 0 or 1 to the current precision, the integer result is returned."
},

{
    "location": "man/09-polynomials.html#Reduce.Algebra.setmod",
    "page": "9 Polynomials and Rationals",
    "title": "Reduce.Algebra.setmod",
    "category": "function",
    "text": "setmod(::Integer)\n\nREDUCE includes facilities for manipulating polynomials whose coefficients are computed modulo a given base. To use this option, two commands must be used; R\"setmod ⟨integer⟩\", to set the prime modulus, and on(:modular) to cause the actual modular calculations to occur. For example, with R\"setmod 3\" and R\"on modular\", the polynomial (a+2*b)^3 would become a^3+2*n^3.\n\nThe argument of setmod is evaluated algebraically, except that non-modular (integer) arithmetic is used. Thus the sequence\n\nR\"setmod 3; on modular; setmod 7\"\n\nwill correctly set the modulus to 7.\n\n\n\n"
},

{
    "location": "man/09-polynomials.html#.11.3-Modular-Number-Coefficients-in-Polynomials-1",
    "page": "9 Polynomials and Rationals",
    "title": "9.11.3 Modular Number Coefficients in Polynomials",
    "category": "section",
    "text": "Reduce.Algebra.setmodModular numbers are by default represented by integers in the interval 0p-1 where p is the current modulus. Sometimes it is more convenient to use an equivalent symmetric representation in the interval -p2+1p2, or more precisely -floor((p-1)2) ceiling((p-1)2), especially if the modular numbers map objects that include negative quantities. The switch balanced_mod allows you to select the symmetric representation for output.Users should note that the modular calculations are on the polynomial coefficients only. It is not currently possible to reduce the exponents since no check for a prime modulus is made (which would allow x^p-1 to be reduced to 1 mod p). Note also that any division by a number not co-prime with the modulus will result in the error “Invalid modular division”."
},

{
    "location": "man/09-polynomials.html#.11.4-Complex-Number-Coefficients-in-Polynomials-1",
    "page": "9 Polynomials and Rationals",
    "title": "9.11.4 Complex Number Coefficients in Polynomials",
    "category": "section",
    "text": "Although REDUCE routinely treats the square of the variable i as equivalent to -1, this is not sufficient to reduce expressions involving i to lowest terms, or to factor such expressions over the complex numbers. For example, in the default case,julia> Algebra.factorize(:(a^2+1))gives the result(:(a ^ 2 + 1), 1)and        (a^2+b^2)/(a+i*b)is not reduced further. However, if the switch complex is turned on, full complex arithmetic is then carried out. In other words, the above factorization will give the result(:(a + im, 1), (a - im, 1))and the quotient will be reduced to a-I*b.The switch complex may be combined with rounded to give complex real numbers; the appropriate arithmetic is performed in this case.Complex conjugation is used to remove complex numbers from denominators of expressions. To do this if complex is off, you must turn the switch rationalize on."
},

{
    "location": "man/09-polynomials.html#.12-ROOT_VAL-Operator-1",
    "page": "9 Polynomials and Rationals",
    "title": "9.12 ROOT_VAL Operator",
    "category": "section",
    "text": "Reduce.Algebra.root_val"
},

{
    "location": "man/10-properties.html#",
    "page": "10 Assigning and Testing Algebraic Properties",
    "title": "10 Assigning and Testing Algebraic Properties",
    "category": "page",
    "text": ""
},

{
    "location": "man/10-properties.html#Assigning-and-Testing-Algebraic-Properties-1",
    "page": "10 Assigning and Testing Algebraic Properties",
    "title": "10 Assigning and Testing Algebraic Properties",
    "category": "section",
    "text": "Sometimes algebraic expressions can be further simplified if there is additional information about the value ranges of its components. The following section describes how to inform REDUCE of such assumptions.Pages = [\"10-properties.md\"]"
},

{
    "location": "man/10-properties.html#Reduce.Algebra.realvalued",
    "page": "10 Assigning and Testing Algebraic Properties",
    "title": "Reduce.Algebra.realvalued",
    "category": "function",
    "text": "realvalued(r...)\n\nThe declaration realvalued may be used to restrict variables to the real numbers. The syntax is:\n\n	Algebra.realvalued(v1,...vn)\n\nFor such variables the operator impart gives the result zero. Thus, with\n\njulia> Algebra.realvalued(:x,:y)\n\nthe expression impart(x+sin(y)) is evaluated as zero. You may also declare an operator as real valued with the meaning, that this operator maps real arguments always to real values. Example:\n\njulia> Algebra.operator(:h); Algebra.realvalued(:h,:x)\n\njulia> Algebra.impart(:(h(x)))\n0  \n \njulia> Algebra.impart(:(h(w)))\n:(impart(h(w)))\n\nSuch declarations are not needed for the standard elementary functions.\n\n\n\n"
},

{
    "location": "man/10-properties.html#Reduce.Algebra.notrealvalued",
    "page": "10 Assigning and Testing Algebraic Properties",
    "title": "Reduce.Algebra.notrealvalued",
    "category": "function",
    "text": "notrealvalued(r...)\n\nTo remove the realvalued propery from a variable or an operator use the declaration notrealvalued with the syntax:\n\njulia> Algebra.notrealvalued(v1,...vn)\n\n\n\n"
},

{
    "location": "man/10-properties.html#.1-REALVALUED-Declaration-and-Check-1",
    "page": "10 Assigning and Testing Algebraic Properties",
    "title": "10.1 REALVALUED Declaration and Check",
    "category": "section",
    "text": "Reduce.Algebra.realvalued\nReduce.Algebra.notrealvaluedThe boolean operator realvaluedp allows you to check if a variable, an operator, or an operator expression is known as real valued. Thus,julia> Algebra.realvalued(:x)\n\njulia> R\"write if realvaluedp(sin x) then ~yes~ else ~no~\"\n\njulia> R\"write if realvaluedp(sin z) then ~yes~ else ~no~\"would print first yes and then no. For general expressions test the impart for checking the value range:julia> Alebra.realvalued(:x,:y); R\"w:=(x+i*y); w1:=conj w\" |> rcall\n\njulia> Algebra.impart(:(w*w1))\n0  \n\njulia> Algebra.impart(:(w*w))\n:(2x*y)"
},

{
    "location": "man/10-properties.html#.2-Declaring-Expressions-Positive-or-Negative-1",
    "page": "10 Assigning and Testing Algebraic Properties",
    "title": "10.2 Declaring Expressions Positive or Negative",
    "category": "section",
    "text": "Detailed knowlege about the sign of expressions allows REDUCE to simplify expressions involving exponentials or abs. You can express assumptions about the positivity or negativity of expressions by rules for the operator sign. Examples:julia> Algebra.abs(:(a*b*c))\n:(abs(a*b*c))\n\njulia> Algebra.rlet((:(sign(a))=>1,:(sign(b))=>1)); :(abs(a*b*c) |> rcall\n:(abs(c) * a * b)\n \njulia> Algebra.on(:precise); Algebra.sqrt(:(x^2-2x+1))\n:(abs(x - 1))\n\nreduce> ws where sign(x-1)=>1;\n\nx - 1Here factors with known sign are factored out of an abs expression.julia> Algebra.on(:precise); Algebra.on(:factor)\n \nreduce> (q*x-2q)^w;  \n\n           w\n((x - 2)*q) \n\nreduce> ws where sign(x-2)=>1;\n\n w        w  \nq *(x - 2)  In this case the factor (x - 2)^w may be extracted from the base of the exponential because it is known to be positive.Note that REDUCE knows a lot about sign propagation. For example, with x and y also x + y, x + y +  and (x + e)y^2 are known as positive. Nevertheless, it is often necessary to declare additionally the sign of a combined expression. E.g. at present a positivity declaration of x- 2 does not automatically lead to sign evaluation for x- 1 or for x."
},

{
    "location": "man/11-substitution.html#",
    "page": "11 Substitution Commands",
    "title": "11 Substitution Commands",
    "category": "page",
    "text": ""
},

{
    "location": "man/11-substitution.html#Substitution-Commands-1",
    "page": "11 Substitution Commands",
    "title": "11 Substitution Commands",
    "category": "section",
    "text": "An important class of commands in REDUCE define substitutions for variables and expressions to be made during the evaluation of expressions. Such substitutions use the prefix operator SUB, various forms of the command let, and rule sets.Pages = [\"11-substitution.md\"]"
},

{
    "location": "man/11-substitution.html#Reduce.Algebra.sub",
    "page": "11 Substitution Commands",
    "title": "Reduce.Algebra.sub",
    "category": "function",
    "text": "sub(::Union{Dict,Pair},expr)\n\nMake variable substitutions using Reduce\'s native sub command. Syntax:\n\nR\"(⟨substitution_list⟩,⟨EXPRN1:algebraic⟩)\"\n\nwhere ⟨substitution_list⟩ is a list of one or more equations of the form\n\n⟨VAR:kernel⟩ = ⟨EXPRN:algebraic⟩\n\nor a kernel that evaluates to such a list.\n\nThe sub operator gives the algebraic result of replacing every occurrence of the variable var in the expression EXPRN1 by the expression EXPRN. Specifically, EXPRN1 is first evaluated using all available rules. Next the substitutions are made, and finally the substituted expression is reevaluated. When more than one variable occurs in the substitution list, the substitution is performed by recursively walking down the tree representing EXPRN1, and replacing every VAR found by the appropriate EXPRN. The EXPRN are not themselves searched for any occurrences of the various VARs. The trivial case sub(EXPRN1)returns the algebraic value ofEXPRN1`.\n\nExamples:\n\n                                    2              2\n     sub({x=a+y,y=y+1},x^2+y^2) -> A  + 2*A*Y + 2*Y  + 2*Y + 1\n\nand with R\"s := {x=a+y,y=y+1}\",\n\n                                    2              2\n     sub(s,x^2+y^2)             -> A  + 2*A*Y + 2*Y  + 2*Y + 1\n\nNote that the global assignments R\"x:=a+y\", etc., do not take place.\n\nEXPRN1 can be any valid algebraic expression whose type is such that a substitution process is defined for it (e.g., scalar expressions, lists and matrices). An error will occur if an expression of an invalid type for substitution occurs either in EXPRN or EXPRN1.\n\n\n\nsub(T::DataType,expr::Expr)\n\nMake a substitution to convert numerical values to type T\n\n\n\n"
},

{
    "location": "man/11-substitution.html#.1-SUB-Operator-1",
    "page": "11 Substitution Commands",
    "title": "11.1 SUB Operator",
    "category": "section",
    "text": "Reduce.Algebra.subThe braces around the substitution list may also be omitted, as in:                                    2              2  \n     sub(x=a+y,y=y+1,x^2+y^2)   -> A  + 2*A*Y + 2*Y  + 2*Y + 1"
},

{
    "location": "man/11-substitution.html#Reduce.Algebra.rlet",
    "page": "11 Substitution Commands",
    "title": "Reduce.Algebra.rlet",
    "category": "function",
    "text": "rlet(::Union{Dict,Pair},expr)\n\nThe simplest use of the let statement is in the form\n\nR\"let ⟨substitution list⟩\"\n\nwhere ⟨substitution list⟩ is a list of rules separated by commas, each of the form:\n\n⟨variable⟩ = ⟨expression⟩\n\nor\n\n⟨prefix operator⟩(⟨argument⟩,…,⟨argument⟩) = ⟨expression⟩\n\nor\n\n⟨argument⟩⟨infix operator⟩,…,⟨argument⟩ = ⟨expression⟩\n\nFor example,\n\n        let {x => y^2,\n             h(u,v) => u - v,\n             cos(pi/3) => 1/2,\n             a*b => c,\n             l+m => n,\n             w^3 => 2*z - 3,\n             z^10 => 0}\n\nThe list brackets can be left out if preferred. The above rules could also have been entered as seven separate let statements.\n\nAfter such let rules have been input, x will always be evaluated as the square of y, and so on. This is so even if at the time the let rule was input, the variable y had a value other than y. (In contrast, the assignment R\"x:=y^2\" will set x equal to the square of the current value of y, which could be quite different.)\n\nThe rule let a*b=c means that whenever a and b are both factors in an expression their product will be replaced by c. For example, a^5*b^7*w would be replaced by c^5*b^2*w.\n\nThe rule for l+m will not only replace all occurrences of l+m by n, but will also normally replace l by n-m, but not m by n-l. A more complete description of this case is given in Section 11.2.5.\n\nThe rule pertaining to w^3 will apply to any power of w greater than or equal to the third.\n\nNote especially the last example, let z^10=0. This declaration means, in effect: ignore the tenth or any higher power of z. Such declarations, when appropriate, often speed up a computation to a considerable degree. (See Section 11.4 for more details.)\n\nAny new operators occurring in such let rules will be automatically declared operator by the system, if the rules are being read from a file. If they are being entered interactively, the system will ask Declare… Operator?. Answer Y or N and hit <Return>.\n\nIn each of these examples, substitutions are only made for the explicit expressions given; i.e., none of the variables may be considered arbitrary in any sense. For example, the command\n\njulia> Algebra.rlet( :(h(u,v)) => :(u - v) )\n\nwill cause h(u,v) to evaluate to u - v, but will not affect h(u,z) or h with any arguments other than precisely the symbols u,v.\n\nThese simple let rules are on the same logical level as assignments made with the := operator. An assignment R\"x := p+q\" cancels a rule rlet( :x => :(y^2) ) made earlier, and vice versa.\n\n\n\n"
},

{
    "location": "man/11-substitution.html#.2-LET-Rules-1",
    "page": "11 Substitution Commands",
    "title": "11.2 LET Rules",
    "category": "section",
    "text": "Unlike substitutions introduced via sub, let rules are global in scope and stay in effect until replaced or cleared.Reduce.Algebra.rletCAUTION: A recursive rule such asjulia> Algebra.rlet( :x => :(x + 1) )is erroneous, since any subsequent evaluation of x would lead to a non-terminating chain of substitutions:      x -> x + 1 -> (x + 1) + 1 -> ((x + 1) + 1) + 1 -> ...Similarly, coupled substitutions such asjulia> Algebra.rlet([:l => :(m + n), :n => :(l + r)])would lead to the same error. As a result, if you try to evaluate an x, l or n defined as above, you will get an error such as        X improperly defined in terms of itselfArray and matrix elements can appear on the left-hand side of a let statement. However, because of their instant evaluation property, it is the value of the element that is substituted for, rather than the element itself. E.g.,        array a(5);  \n        a(2) := b;  \n        let a(2) = c;results in b being substituted by c; the assignment for a(2) does not change.Finally, if an error occurs in any equation in a let statement (including generalized statements involving for all and such that), the remaining rules are not evaluated."
},

{
    "location": "man/11-substitution.html#.2.1-FOR-ALL-…-LET-1",
    "page": "11 Substitution Commands",
    "title": "11.2.1 FOR ALL … LET",
    "category": "section",
    "text": "If a substitution for all possible values of a given argument of an operator is required, the declaration FOR ALL may be used. The syntax of such a command isR\"for all ⟨variable⟩,…,⟨variable⟩ ⟨LET statement⟩⟨terminator⟩\"e.g.,R\"for all x,y let h(x,y) = x-y\"\nR\"for all x let k(x,y) = x^y\"The first of these declarations would cause h(a,b) to be evaluated as a-b, h(u+v,u+w) to be v-w, etc. If the operator symbol h is used with more or fewer argument places, not two, the let would have no effect, and no error would result.The second declaration would cause k(a,y) to be evaluated as a^y, but would have no effect on k(a,z) since the rule didn’t say for all y….Where we used x and y in the examples, any variables could have been used. This use of a variable doesn’t affect the value it may have outside the let statement. However, you should remember what variables you actually used. If you want to delete the rule subsequently, you must use the same variables in the clear command.It is possible to use more complicated expressions as a template for a let statement, as explained in the section on substitutions for general expressions. In nearly all cases, the rule will be accepted, and a consistent application made by the system. However, if there is a sole constant or a sole free variable on the left-hand side of a rule (e.g., R\"let 2=3 or for all x let x=2\"), then the system is unable to handle the rule, and the error message        Substitution for ... not allowedwill be issued. Any variable listed in the for all part will have its symbol preceded by an equal sign: x in the above example will appear as =x. An error will also occur if a variable in the for all part is not properly matched on both sides of the let equation."
},

{
    "location": "man/11-substitution.html#.2.2-FOR-ALL-…-SUCH-THAT-…-LET-1",
    "page": "11 Substitution Commands",
    "title": "11.2.2 FOR ALL … SUCH THAT … LET",
    "category": "section",
    "text": "If a substitution is desired for more than a single value of a variable in an operator or other expression, but not all values, a conditional form of the for all … let declaration can be used.Example:R\"for all x such that numberp x and x<0 let h(x)=0\"will cause h(-5) to be evaluated as 0, but h of a positive integer, or of an argument that is not an integer at all, would not be affected. Any boolean expression can follow the such that keywords."
},

{
    "location": "man/11-substitution.html#Reduce.Algebra.clear",
    "page": "11 Substitution Commands",
    "title": "Reduce.Algebra.clear",
    "category": "function",
    "text": "clear(r...)\n\nThe user may remove all assignments and substitution rules from any expression by the command clear, in the form\n\nR\"clear ⟨expression⟩,…,⟨expression⟩ = ⟨terminator⟩\"\ne.g.\n\nJulia julia> Algebra.clear(:x,:(h(x,y)))\n\nBecause of their *instant evaluation* property, array and matrix elements cannot be cleared with `clear`. For example, if `a` is an array, you must say\n\nJulia R\"a(3) := 0\"\n\nrather than\n\nJulia R\"clear a(3)\" ``to “clear” elementa(3)`.\n\nOn the other hand, a whole array (or matrix) a can be cleared by the command clear(:a). This means much more than resetting to 0 all the elements of a. The fact that a is an array, and what its dimensions are, are forgotten, so a can be redefined as another type of object, for example an operator.\n\nIf you need to clear a variable whose name must be computed, see the unset statement.\n\n\n\n"
},

{
    "location": "man/11-substitution.html#.2.3-Removing-Assignments-and-Substitution-Rules-1",
    "page": "11 Substitution Commands",
    "title": "11.2.3 Removing Assignments and Substitution Rules",
    "category": "section",
    "text": "Reduce.Algebra.clearThe more general types of let declarations can also be deleted by using clear. Simply repeat the let rule to be deleted, using clear in place of let, and omitting the equal sign and right-hand part. The same dummy variables must be used in the for all part, and the boolean expression in the such  that part must be written the same way. (The placing of blanks doesn’t have to be identical.)Example: The let ruleR\"for all x such that numberp x and x<0 let h(x)=0\"can be erased by the commandR\"for all x such that numberp x and x<0 clear h(x)\""
},

{
    "location": "man/11-substitution.html#.2.4-Overlapping-LET-Rules-1",
    "page": "11 Substitution Commands",
    "title": "11.2.4 Overlapping LET Rules",
    "category": "section",
    "text": "clear is not the only way to delete a let rule. A new let rule identical to the first, but with a different expression after the equal sign, replaces the first. Replacements are also made in other cases where the existing rule would be in conflict with the new rule. For example, a rule for x^4 would replace a rule for x^5. The user should however be cautioned against having several let rules in effect that relate to the same expression. No guarantee can be given as to which rules will be applied by REDUCE or in what order. It is best to clear an old rule before entering a new related let rule."
},

{
    "location": "man/11-substitution.html#.2.5-Substitutions-for-General-Expressions-1",
    "page": "11 Substitution Commands",
    "title": "11.2.5 Substitutions for General Expressions",
    "category": "section",
    "text": "The examples of substitutions discussed in other sections have involved very simple rules. However, the substitution mechanism used in REDUCE is very general, and can handle arbitrarily complicated rules without difficulty.The general substitution mechanism used in REDUCE is discussed in Hearn, A. C., “REDUCE, A User-Oriented Interactive System for Algebraic Simplification,” Interactive Systems for Experimental Applied Mathematics, (edited by M. Klerer and J. Reinfelds), Academic Press, New York (1968), 79-90, and Hearn. A. C., “The Problem of Substitution,” Proc. 1968 Summer Institute on Symbolic Mathematical Computation, IBM Programming Laboratory Report FSC 69-0312 (1969). For the reasons given in these references, REDUCE does not attempt to implement a general pattern matching algorithm. However, the present system uses far more sophisticated techniques than those discussed in the above papers. It is now possible for the rules appearing in arguments of let to have the form⟨substitution expression⟩ = ⟨expression⟩where any rule to which a sensible meaning can be assigned is permitted. However, this meaning can vary according to the form of ⟨substitution expression⟩. The semantic rules associated with the application of the substitution are completely consistent, but somewhat complicated by the pragmatic need to perform such substitutions as efficiently as possible. The following rules explain how the majority of the cases are handled.To begin with, the ⟨substitution expression⟩ is first partly simplified by collecting like terms and putting identifiers (and kernels) in the system order. However, no substitutions are performed on any part of the expression with the exception of expressions with the instant evaluation property, such as array and matrix elements, whose actual values are used. It should also be noted that the system order used is not changeable by the user, even with the korder command. Specific cases are then handled as follows:If the resulting simplified rule has a left-hand side that is an identifier, an expression with a top-level algebraic operator or a power, then the rule is added without further change to the appropriate table.\nIf the operator * appears at the top level of the simplified left-hand side, then any constant arguments in that expression are moved to the right-hand side of the rule. The remaining left-hand side is then added to the appropriate table. For example,julia> Algebra.rlet(:(2*x*y) => 3)becomesjulia> Algebra.rlet(:(x*y) => 3/2)so that x*y is added to the product substitution table, and when this rule is applied, the expression x*y becomes 3/2, but x or y by themselves are not replaced.If the operators +, - or / appear at the top level of the simplified left-hand side, all but the first term is moved to the right-hand side of the rule. Thus the rulesjulia> Algebra.rlet(:(l+m)=>:n, :(x/2)=>:y, :(a-b)=>:c)becomejulia> Algebra.rlet(:l=>:(n-m), :x=>:(2*y), :a=:(c+b))One problem that can occur in this case is that if a quantified expression is moved to the right-hand side, a given free variable might no longer appear on the left-hand side, resulting in an error because of the unmatched free variable. E.g.,R\"for all x,y let f(x)+f(y)=x*y\"would becomeR\"for all x,y let f(x)=x*y-f(y)\"which no longer has y on both sides.The fact that array and matrix elements are evaluated in the left-hand side of rules can lead to confusion at times. Consider for example the statementsR\"array a(5); let x+a(2)=3; let a(3)=4\"The left-hand side of the first rule will become x, and the second 0. Thus the first rule will be instantiated as a substitution for x, and the second will result in an error.The order in which a list of rules is applied is not easily understandable without a detailed knowledge of the system simplification protocol. It is also possible for this order to change from release to release, as improved substitution techniques are implemented. Users should therefore assume that the order of application of rules is arbitrary, and program accordingly.After a substitution has been made, the expression being evaluated is reexamined in case a new allowed substitution has been generated. This process is continued until no more substitutions can be made.As mentioned elsewhere, when a substitution expression appears in a product, the substitution is made if that expression divides the product. For example, the rulejulia> Algebra.rlet(:(a^2*c) => :(3*z))would cause a^2*c*x to be replaced by 3*z*x and a^2*c^2 by 3*z*c. If the substitution is desired only when the substitution expression appears in a product with the explicit powers supplied in the rule, the command match should be used instead.For example,R\"match a^2*c = 3*z\"would cause a^2*c*x to be replaced by 3*z*x, but a^2*c^2 would not be replaced. match can also be used with the for all constructions described above.To remove substitution rules of the type discussed in this section, the clear command can be used, combined, if necessary, with the same for all clause with which the rule was defined, for example:R\"for all x clear log(e^x),e^log(x),cos(w*t+theta(x))\"Note, however, that the arbitrary variable names in this case must be the same as those used in defining the substitution."
},

{
    "location": "man/11-substitution.html#Reduce.Algebra.clearrules",
    "page": "11 Substitution Commands",
    "title": "Reduce.Algebra.clearrules",
    "category": "function",
    "text": "clearrules(r)\n\nclearrules has the syntax\n\nR\"clearrules <rule list>|<name of rule list>(,...)\"\n\n\n\n"
},

{
    "location": "man/11-substitution.html#.3-Rule-Lists-1",
    "page": "11 Substitution Commands",
    "title": "11.3 Rule Lists",
    "category": "section",
    "text": "Rule lists offer an alternative approach to defining substitutions that is different from either sub or let. In fact, they provide the best features of both, since they have all the capabilities of let, but the rules can also be applied locally as is possible with sub. In time, they will be used more and more in REDUCE. However, since they are relatively new, much of the REDUCE code you see uses the older constructs.A rule list is a list of rules that have the syntax     <expression> => <expression> (WHEN <boolean expression>)For example,        {cos(~x)*cos(~y) => (cos(x+y)+cos(x-y))/2,  \n         cos(~n*pi)      => (-1)^n when remainder(n,2)=0}The tilde preceding a variable marks that variable as free for that rule, much as a variable in a for all clause in a let statement. The first occurrence of that variable in each relevant rule must be so marked on input, otherwise inconsistent results can occur. For example, the rule list        {cos(~x)*cos(~y) => (cos(x+y)+cos(x-y))/2,  \n         cos(x)^2        => (1+cos(2x))/2}designed to replace products of cosines, would not be correct, since the second rule would only apply to the explicit argument x. Later occurrences in the same rule may also be marked, but this is optional (internally, all such rules are stored with each relevant variable explicitly marked). The optional when clause allows constraints to be placed on the application of the rule, much as the such that clause in a let statement.A rule list may be named, for example        trig1 := {cos(~x)*cos(~y) => (cos(x+y)+cos(x-y))/2,  \n                  cos(~x)*sin(~y) => (sin(x+y)-sin(x-y))/2,  \n                  sin(~x)*sin(~y) => (cos(x-y)-cos(x+y))/2,  \n                  cos(~x)^2       => (1+cos(2*x))/2,  \n                  sin(~x)^2       => (1-cos(2*x))/2};Such named rule lists may be inspected as needed. E.g., the command R trig1\" would cause the above list to be printed.Rule lists may be used in two ways. They can be globally instantiated by means of the command let. For example,julia> Algebra.rlet(:trig1)would cause the above list of rules to be globally active from then on until cancelled by the command clearrules, as injulia> Algebra.clearrules(:trig1)Reduce.Algebra.clearrulesThe second way to use rule lists is to invoke them locally by means of a where clause. For example        cos(a)*cos(b+c)  \n           where {cos(~x)*cos(~y) => (cos(x+y)+cos(x-y))/2};orR\"cos(a)*sin(b) where trigrules\"The syntax of an expression with a where clause is:        <expression>  \n            WHERE <rule>|<rule list>(,<rule>|<rule list> ...)so the first example above could also be written        cos(a)*cos(b+c)  \n           where cos(~x)*cos(~y) => (cos(x+y)+cos(x-y))/2;The effect of this construct is that the rule list(s) in the where clause only apply to the expression on the left of where. They have no effect outside the expression. In particular, they do not affect previously defined where clauses or let statements. For example, the sequence     let a=2;  \n     a where a=>4;  \n     a;would result in the output     4  \n \n     2Although where has a precedence less than any other infix operator, it still binds higher than keywords such as else, then, do, repeat and so on. Thus the expressionR\"if a=2 then 3 else a+2 where a=3\"will parse asR\"if a=2 then 3 else (a+2 where a=3)\"where may be used to introduce auxiliary variables in symbolic mode expressions, as described in Section 17.4. However, the symbolic mode use has different semantics, so expressions do not carry from one mode to the other.Compatibility Note: In order to provide compatibility with older versions of rule lists released through the Network Library, it is currently possible to use an equal sign interchangeably with the replacement sign => in rules and let statements. However, since this will change in future versions, the replacement sign is preferable in rules and the equal sign in non-rule-based let statements."
},

{
    "location": "man/11-substitution.html#Advanced-Use-of-Rule-Lists-1",
    "page": "11 Substitution Commands",
    "title": "Advanced Use of Rule Lists",
    "category": "section",
    "text": "Some advanced features of the rule list mechanism make it possible to write more complicated rules than those discussed so far, and in many cases to write more compact rule lists. These features are:Free operators\nDouble slash operator\nDouble tilde variables.A free operator in the left hand side of a pattern will match any operator with the same number of arguments. The free operator is written in the same style as a variable. For example, the implementation of the product rule of differentiation can be written as:operator diff, !~f, !~g;  \n \nprule := {diff(~f(~x) * ~g(~x),x) =>  \n             diff(f(x),x) * g(x) + diff(g(x),x) * f(x)};  \n \nlet prule;  \n \ndiff(sin(z)*cos(z),z);  \n \n         cos(z)*diff(sin(z),z) + diff(cos(z),z)*sin(z)The double slash operator may be used as an alternative to a single slash (quotient) in order to match quotients properly. E.g., in the example of the Gamma function above, one can use:gammarule :=  \n   {gamma(~z)//(~c*gamma(~zz))  => gamma(z)/(c*gamma(zz-1)*zz)  \n                  when fixp(zz -z) and (zz -z) >0,  \n    gamma(~z)//gamma(~zz) => gamma(z)/(gamma(zz-1)*zz)  \n                  when fixp(zz -z) and (zz -z) >0};  \n \nlet gammarule;  \n \ngamma(z)/gamma(z+3);  \n \n          1  \n----------------------  \n  3      2  \n z  + 6*z  + 11*z + 6The above example suffers from the fact that two rules had to be written in order to perform the required operation. This can be simplified by the use of double tilde variables. E.g. the rule list GGrule :=  {  \n    gamma(~z)//(~~c*gamma(~zz))  => gamma(z)/(c*gamma(zz-1)*zz)  \n     when fixp(zz -z) and (zz -z) >0};will implement the same operation in a much more compact way. In general, double tilde variables are bound to the neutral element with respect to the operation in which they are used.Pattern given	Argument used	Binding\n~z + ~~y	x	z=x; y=0\n~z + ~~y	x+3	z=x; y=3 or z=3; y=x\n~z * ~~y	x	z=x; y=1\n~z * ~~y	x*3	z=x; y=3 or z=3; y=x\n~z / ~~y	x	z=x; y=1\n~z / ~~y	x/3	z=x; y=3Remarks: A double tilde variable as the numerator of a pattern is not allowed. Also, using double tilde variables may lead to recursion errors when the zero case is not handled properly.let f(~~a * ~x,x)  => a * f(x,x) when freeof (a,x);  \n \nf(z,z);  \n \n***** f(z,z) improperly defined in terms of itself  \n \n% BUT:  \n \nlet ff(~~a * ~x,x)  \n       => a * ff(x,x) when freeof (a,x) and a neq 1;  \n \nff(z,z);  \n                 ff(z,z)  \n \nff(3*z,z);  \n                 3*ff(z,z)"
},

{
    "location": "man/11-substitution.html#Reduce.Algebra.showrules",
    "page": "11 Substitution Commands",
    "title": "Reduce.Algebra.showrules",
    "category": "function",
    "text": "showrules(r)\n\nThe operator showrules takes a single identifier as argument, and returns in rule-list form the operator rules associated with that argument. For example:\n\nreduce> showrules log;  \n \n{log(e) => 1,  \n \n log(1) => 0,  \n \n      ~x  \n log(e  ) => ~x,  \n \n                    1  \n df(log(~x),~x) => ----}  \n                    ~x\n\nSuch rules can then be manipulated further as with any list. For example R\"rhs first ws\" has the value 1. Note that an operator may have other properties that cannot be displayed in such a form, such as the fact it is an odd function, or has a definition defined as a procedure.\n\n\n\n"
},

{
    "location": "man/11-substitution.html#Displaying-Rules-Associated-with-an-Operator-1",
    "page": "11 Substitution Commands",
    "title": "Displaying Rules Associated with an Operator",
    "category": "section",
    "text": "Reduce.Algebra.showrules"
},

{
    "location": "man/11-substitution.html#Order-of-Application-of-Rules-1",
    "page": "11 Substitution Commands",
    "title": "Order of Application of Rules",
    "category": "section",
    "text": "If rules have overlapping domains, their order of application is important. In general, it is very difficult to specify this order precisely, so that it is best to assume that the order is arbitrary. However, if only one operator is involved, the order of application of the rules for this operator can be determined from the following:Rules containing at least one free variable apply before all rules without free variables.\nRules activated in the most recent let command are applied first.\nlet with several entries generate the same order of application as a corresponding sequence of commands with one rule or rule set each.\nWithin a rule set, the rules containing at least one free variable are applied in their given order. In other words, the first member of the list is applied first.\nConsistent with the first item, any rule in a rule list that contains no free variables is applied after all rules containing free variables.Example: The following rule set enables the computation of exact values of the Gamma function:        operator gamma,gamma_error;  \n        gamma_rules :=  \n        {gamma(~x)=>sqrt(pi)/2 when x=1/2,  \n         gamma(~n)=>factorial(n-1) when fixp n and n>0,  \n         gamma(~n)=>gamma_error(n) when fixp n,  \n         gamma(~x)=>(x-1)*gamma(x-1) when fixp(2*x) and x>1,  \n         gamma(~x)=>gamma(x+1)/x when fixp(2*x)};Here, rule by rule, cases of known or definitely uncomputable values are sorted out; e.g. the rule leading to the error expression will be applied for negative integers only, since the positive integers are caught by the preceding rule, and the last rule will apply for negative odd multiples of 1∕2 only. Alternatively the first rule could have been written as        gamma(1/2) => sqrt(pi)/2but then the case x = 12 should be excluded in the when part of the last rule explicitly because a rule without free variables cannot take precedence over the other rules."
},

{
    "location": "man/11-substitution.html#.4-Asymptotic-Commands-1",
    "page": "11 Substitution Commands",
    "title": "11.4 Asymptotic Commands",
    "category": "section",
    "text": "In expansions of polynomials involving variables that are known to be small, it is often desirable to throw away all powers of these variables beyond a certain point to avoid unnecessary computation. The command let may be used to do this. For example, if only powers of x up to x^7 are needed, the commandjulia> Algebra.rlet(:(x^8) => 0)will cause the system to delete all powers of x higher than 7.CAUTION: This particular simplification works differently from most substitution mechanisms in REDUCE in that it is applied during polynomial manipulation rather than to the whole evaluated expression. Thus, with the above rule in effect, x^10/x^5 would give the result zero, since the numerator would simplify to zero. Similarly x^20/x^10 would give a Zero divisor error message, since both numerator and denominator would first simplify to zero.The method just described is not adequate when expressions involve several variables having different degrees of smallness. In this case, it is necessary to supply an asymptotic weight to each variable and count up the total weight of each product in an expanded expression before deciding whether to keep the term or not. There are two associated commands in the system to permit this type of asymptotic constraint. The command WEIGHT takes a list of equations of the form⟨kernel form⟩ = ⟨number⟩where ⟨number⟩ must be a positive integer (not just evaluate to a positive integer). This command assigns the weight ⟨number⟩ to the relevant kernel form. A check is then made in all algebraic evaluations to see if the total weight of the term is greater than the weight level assigned to the calculation. If it is, the term is deleted. To compute the total weight of a product, the individual weights of each kernel form are multiplied by their corresponding powers and then added.The weight level of the system is initially set to 1. The user may change this setting by the commandR\"wtlevel <number>\"which sets ⟨number⟩ as the new weight level of the system. meta must evaluate to a positive integer. wtlevel will also allow nil as an argument, in which case the current weight level is returned."
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
