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
    "text": "Reduce.ResetRExprrcallparseload_packagesubsquash"
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
    "page": "-",
    "title": "-",
    "category": "page",
    "text": "#1 Introductory InformationREDUCE is a system for carrying out algebraic operations accurately, no matter how complicated the expressions become. It can manipulate polynomials in a variety of forms, both expanding and factoring them, and extract various parts of them as required. REDUCE can also do differentiation and integration, but we shall only show trivial examples of this in this introduction. Other topics not considered include the use of arrays, the definition of procedures and operators, the specific routines for high energy physics calculations, the use of files to eliminate repetitious typing and for saving results, and the editing of the input text.Also not considered in any detail in this introduction are the many options that are available for varying computational procedures, output forms, number systems used, and so on.REDUCE is designed to be an interactive system, so that the user can input an algebraic expression and see its value before moving on to the next calculation. For those systems that do not support interactive use, or for those calculations, especially long ones, for which a standard script can be defined, REDUCE can also be used in batch mode. In this case, a sequence of commands can be given to REDUCE and results obtained without any user interaction during the computation.In this introduction, we shall limit ourselves to the interactive use of REDUCE, since this illustrates most completely the capabilities of the system. When REDUCE is called, it begins by printing a banner message like:julia> using Reduce\nReduce (Free CSL version, revision 4521),  11-March-2018 ...where the version number and the system release date will change from time to time in deps/svn.jl. To enter the Reduce REPL, type <shift>+<]> as the first character in the Julia prompt.reduce>You can now type a REDUCE statement, terminated by a semicolon to indicate the end of the expression, for example:reduce> (x+y+z)^2;This expression would normally be followed by another character (a <Return> on an ASCII keyboard) to “wake up” the system, which would then input the expression, evaluate it, and return the result:           2\n(y + z + x)Note that in Julia the expand switch is disabled by default, unlike the standalone version.Let us review this simple example to learn a little more about the way that REDUCE works. First, we note that REDUCE deals with variables, and constants like other computer languages, but that in evaluating the former, a variable can stand for itself. Expression evaluation normally follows the rules of high school algebra, so the only surprise in the above example might be that the expression was expanded. REDUCE normally expands expressions where possible, collecting like terms and ordering the variables in a specific manner. However, expansion, ordering of variables, format of output and so on is under control of the user, and various declarations are available to manipulate these.Another characteristic of the above example is the use of lower case on input and upper case on output. In fact, input may be in either mode, but output is usually in lower case. To make the difference between input and output more distinct in this manual, all expressions intended for input will be shown in lower case and output in upper case. However, for stylistic reasons, we represent all single identifiers in the text in upper case.Finally, the numerical prompt can be used to reference the result in a later computation. As a further illustration of the system features, the user should try:reduce> for i:= 1:40 product i;The result in this case is the value of 40!,815915283247897734345611269596115894272000000000You can also get the same result by sayingreduce> factorial 40;Since we want exact results in algebraic calculations, it is essential that integer arithmetic be performed to arbitrary precision, as in the above example. Furthermore, the for statement in the above is illustrative of a whole range of combining forms that REDUCE supports for the convenience of the user.Among the many options in REDUCE is the use of other number systems, such as multiple precision floating point with any specified number of digits — of use if roundoff in, say, the 100th digit is all that can be tolerated.In many cases, it is necessary to use the results of one calculation in succeeding calculations. One way to do this is via an assignment for a variable, such asreduce> u := (x+y+z)^2;If we now use u in later calculations, the value of the right-hand side of the above will be used.The results of a given calculation are also saved in the variable ws (for WorkSpace), so this can be used in the next calculation for further processing.For example, the expressionreduce> df(ws,x);following the previous evaluation will calculate the derivative of (x+y+z)^2 with respect to x. Alternatively,reduce> int(ws,y);would calculate the integral of the same expression with respect to y. REDUCE is also capable of handling symbolic matrices. For example,reduce> matrix m(2,2);declares m to be a two by two matrix, andreduce> m := mat((a,b),(c,d));gives its elements values. Expressions that include m and make algebraic sense may now be evaluated, such as 1/m to give the inverse, 2*m - u*m^2 to give us another matrix and det(m) to give us the determinant of m.REDUCE has a wide range of substitution capabilities. The system knows about elementary functions, but does not automatically invoke many of their well-known properties. For example, products of trigonometrical functions are not converted automatically into multiple angle expressions, but if the user wants this, he can say, for example:reduce> (sin(a+b)+cos(a+b))*(sin(a-b)-cos(a-b))  \n            where cos(~x)*cos(~y) => (cos(x+y)+cos(x-y))/2,  \n                  cos(~x)*sin(~y) => (sin(x+y)-sin(x-y))/2,  \n                  sin(~x)*sin(~y) => (cos(x-y)-cos(x+y))/2;where the tilde in front of the variables x and y indicates that the rules apply for all values of those variables. The result of this calculation is - (cos(2*a) + sin(2*b))See also the user-contributed packages ASSIST (chapter 16.5), CAMAL (chapter 16.10) and TRIGSIMP (chapter 16.72).Another very commonly used capability of the system, and an illustration of one of the many output modes of REDUCE, is the ability to output results in a FORTRAN compatible form. Such results can then be used in a FORTRAN based numerical calculation. This is particularly useful as a way of generating algebraic formulas to be used as the basis of extensive numerical calculations.For example, the statementsreduce> on fort;  \nreduce> df(log(x)*(sin(x)+cos(x))/sqrt(x),x,2);will result in the output      ans=(-(4.0*cos(x)*log(x)*x**2+4.0*cos(x)*log(x)*x-3.0*cos(x)*\n     . log(x)-8.0*cos(x)*x+8.0*cos(x)+4.0*log(x)*sin(x)*x**2-4.0*log(\n     . x)*sin(x)*x-3.0*log(x)*sin(x)+8.0*sin(x)*x+8.0*sin(x)))/(4.0*\n     . sqrt(x)*x**2)These algebraic manipulations illustrate the algebraic mode of REDUCE. REDUCE is based on Standard Lisp. A symbolic mode is also available for executing Lisp statements. These statements follow the syntax of Lisp, e.g.reduce> symbolic car ’(a);Communication between the two modes is possible.With this simple introduction, you are now in a position to study the material in the full REDUCE manual in order to learn just how extensive the range of facilities really is. If further tutorial material is desired, the seven REDUCE Interactive Lessons by David R. Stoutemyer are recommended. These are normally distributed with the system."
},

{
    "location": "man/20-maintaining.html#",
    "page": "-",
    "title": "-",
    "category": "page",
    "text": "#20 Maintaining REDUCESince January 1, 2009 REDUCE is Open Source Software. It is hosted athttp://reduce-algebra.sourceforge.net/We mention here three ways in which REDUCE is maintained. The first is the collection of queries, observations and bug-reports. All users are encouraged to subscribe to the mailing list that Sourceforge.net provides so that they will receive information about updates and concerns. Also on SourceForge there is a bug tracker and a forum. The expectation is that the maintainers and keen users of REDUCE will monitor those and try to respond to issues. However these resources are not there to seek answers to Maths homework problems - they are intended specifically for issues to do with the use and support of REDUCE.The second level of support is provided by the fact that all the sources of REDUCE are available, so any user who is having difficulty either with a bug or understanding system behaviour can consult the code to see if (for instance) comments in it clarify something that was unclear from the regular documentation.The source files for REDUCE are available on SourceForge in the Subversion repository. Check the \"code/SVN\" tab on the SourceForge page to find instructions for using a Subversion client to fetch the most up to date copy of everything. From time to time there may be one-file archives of a snapshot of the sources placed in the download area on SourceForge, and eventually some of these mat be marked as “stable” releases, but at present it is recommended that developers use a copy from the Subversion repository.The files fetched there come with a directory called “trunk” that holds the main current REDUCE, and one called “branches” that is reserved for future experimental versions. All the files that we have for creating help files and manuals should also be present in the files you fetch.The packages that make up the source for the algebraic capabilities of REDUCE are in the “packages” sub-directory, and often there are test files for a package present there and especially for contributed packages there will be documentation in the form of a LATEX file. Although REDUCE is coded in its own language many people in the past have found that it does not take too long to start to get used to it.In various cases even fairly “ordinary end users” may wish to fetch the source version of REDUCE and compile it all for themselves. This may either be because they need the benefit of a bug-fix only recently checked into the subversion repository or because no pre-compiled binary is available for the particular computer and operating system they use. This latter is to some extent unavoidable since REDUCE can run on both 32 and 64-bit Windows, the various MacOSX options (eg Intel and Powerpc), many different distributions of Linux, some BSD variants and Solaris (at least). It is not practically feasible for us to provide a constant stream of up to date ready-built binaries for all these.There are instructions for compiling REDUCE present at the top of the trunk source tree. Usually the hardest issue seems to be encuring that your computer has an adequate set of development tools and libraries installled before you start, but once that is sorted out the hope is that the compilation of REDUCE should proceed uneventfully if sometimes tediously.In a typical Open Source way the hope is that some of those who build REDUCE from source or explore the source (out of general interest or to pursue an understanding of some bug or detail) will transform themselves into contributors or developers which moves on to the third level of support.At this third level any user can contribute proposals for bug fixes or extensions to REDUCE or its documentation. It might be valuable to collect a library of additional user-contributed examples illustrating the use of the system too. To do this first ensure that you have a fully up to date copy of the sources from Subversion, and then depending on just what sort of change is being proposed provide the updates to the developers via the SourceForge bug tracker or other route. In time we may give more concrete guidance about the format of changes that will be easiest to handle. It is obviously important that proposed changes have been properly tested and that they are accompanied with a clear explanation of why they are of benefit. A specific concern here is that in the past fixes to a bug in one part of REDUCE have had bad effects on some other applications and packages, so some degree of caution is called for. Anybody who develops a significant whole new package for REDUCE is encouraged to make the developers aware so that it can be considered for inclusion.So the short form explanation about Support and Maintenance is that it is mainly focussed around the SourceForge system. That if discussions about bugs, requirements or issues are conducted there then all users and potential users of REDUCE will be able to benefit from reviewing them, and the Sourceforge mailing lists, tracker, forums and wiki will grow to be both a static repository of answers to common questions, an active set of locations to to get new issues looked at and a focus for guiding future development."
},

{
    "location": "man/A-reserved.html#",
    "page": "-",
    "title": "-",
    "category": "page",
    "text": "#Appendix A: Reserved IdentifiersWe list here all identifiers that are normally reserved in REDUCE including names of commands, operators and switches initially in the system. Excluded are words that are reserved in specific implementations of the system.Commandsalgebraic antisymmetric array bye clear clearrules comment cont decompose define depend display ed editdef end even factor for forall foreach go goto if in index infix input integer korder let linear lisp listargp load load_package mass match matrix matrixproc mshell nodepend noncom nonzero nospur odd off on operator order out pause precedence print_precision procedure quit real remember remfac remind retry return saveas scalar setmod share showtime shut spur symbolic symmetric unset vecdim vector weight write wtlevelBoolean Operatorsevenp fixp freeof numberp ordp primepInfix Operators:= = >= > <= < => + - * / // ^ ** . .. where setq or and member memq equal neq eq geq greaterp leq lessp plus difference minus times quotient recip expt consNumerical Operatorsabs acos acosh acot acoth acsc acsch airy_ai airy_aiprime airy_bi airy_biprime asec asech asin asinh atan atanh atan2 bernoulli besseli besselj besselk bessely beta cos cosh cot coth csc csch csch exp factorial fix floor gamma hankel1 hankel2 hypot ibeta igamma kummerm kummeru lerch_phi ln log logb log10 lommel1 lommel2 nextprime pochhammer polygamma psi round sec sech sin sinh sqrt struveh struvel tan tanh whittakerm whittakeru zetaPrefix Operatorsappend arbcomplex arbint arglength ceiling ci coeff coeffn cofactor conj continued_fraction deg den det df dilog ei eps erf expand_cases factorize fibonacci fibonaccip first gcd g hypergeometric impart int interpol lcm lcof length lhs linelength list lpower lterm mainvar map mat mateigen max meijerg min mkid motzkin nullspace num one_of part pf precision prod random random_new_seed rank rederr reduct remainder repart rest resultant reverse rhs root_of root_val second select set showrules si sign solve solidharmonicy sphericalharmonicy structr sub sum third totaldeg tp trace varnameReserved Variables_line_ assumptions card_no catalan e euler_gamma eval_mode fort_width golden_ratio high_pow i infinity k!* khinchin low_pow negative nil pi positive requirements root_multiplicities tSwitchesadjprec algint allbranch allfac allowdfint arbvars balance_mod bezout bfspace combineexpt combinelogs commutedf comp complex cramer cref defn demo dfint div echo errcont evallhseqp exp expanddf expandlogs ezgcd factor failhard fort fortupper fullroots gcd ifactor int intstr lcm list listargs mcd modular msg multiplicities nat nero nocommutedf noconvert nolnr nosplit output period precise precise_complex pret pri rat ratarg rational rationalize ratpri revpri rlisp88 roundall roundbf rounded savestructr simpnoncomdf solvesingular time tra trdefint trfac trigform trint varoptOtherReservedIdsbegin do then expr fexpr input lambda lisp macro product repeat smacro sum then until when while ws"
},

{
    "location": "man/B-bibliography.html#",
    "page": "-",
    "title": "-",
    "category": "page",
    "text": "#Appendix B: Bibliography[1] Sandra Fillebrown. Faster computation of bernoulli numbers. Journal of Algorithms, 13:431–445, 1992.[2] Wolfram Koepf, Power Series in Computer Algebra, J. Symbolic Computation 13 (1992)"
},

{
    "location": "man/C-changelog.html#",
    "page": "-",
    "title": "-",
    "category": "page",
    "text": "#Appendix C: Changes since Version 3.8New packages assert bibasis breduce cde cdiff clprl gcref guardian lalr lessons libreduce listvecops lpdo redfront reduce4 sstools utf8Core package rlisp Support for namespaces (::)Default value in switch statementSupport for utf8 charactersCore package poly Improvements for differentiation: new switches expanddf, allowdfint etc (from odesolve)Core package alg New switch precise_complexImprovements for switch combineexpt (exptchk.red)New command unsetNew operators continued_fraction, totaldegOperators now defined in the REDUCE core:changevar, si, ci, gamma, igamma, psi, polygamma, beta, ibeta, euler, bernoulli, pochhammer, lerch_phi, polylog, zeta, besselj, bessely, besseli, besselk, hankel1, hankel2, kummerM, kummerU, struveh, struvel, lommel1, lommel2, whittakerm, whittakerw, Airy_Ai, Airy_Bi, Airy_AiPrime, Airy_biprime, binomial, solidharmonic, sphericalharmonic, fibonacci,fibonaccip, motzkin, hypergeometric, MeijerG.Constants now part of the core:now known as part of the core, as well as constants catalan, euler_gamma, golden_ratio, khinchin.Core Package solve New boolean operator polyp(p,var), to determine whether p is a pure polynomial in var, ie. the coefficients of p do not contain var.Core Package matrix New keyword matrixproc for declaration of matrix-valued procedures.Package defint Added trdefint switch for tracing."
},

]}
