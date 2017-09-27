var documenterSearchIndex = {"docs": [

{
    "location": "index.html#",
    "page": "Home",
    "title": "Home",
    "category": "page",
    "text": ""
},

{
    "location": "index.html#Reduce.jl-Documentation-1",
    "page": "Home",
    "title": "Reduce.jl Documentation",
    "category": "section",
    "text": ""
},

{
    "location": "index.html#Reduce.Reset",
    "page": "Home",
    "title": "Reduce.Reset",
    "category": "Function",
    "text": "Reduce.Reset() Kills the REDUCE process and starts a new instance.\n\nExamples\n\njulia> Reduce.Reset()\nReduce (Free PSL version, revision 4015),  5-May-2017 ...\n\n\n\n"
},

{
    "location": "index.html#Reduce.RExpr",
    "page": "Home",
    "title": "Reduce.RExpr",
    "category": "Type",
    "text": "A Reduce expression\n\nSummary:\n\ntype RExpr <: Any\n\nFields:\n\nstr :: Array{Compat.String,1}\n\n\n\n"
},

{
    "location": "index.html#Base.parse",
    "page": "Home",
    "title": "Base.parse",
    "category": "Function",
    "text": "parse(rexpr::RExpr) Parse a Reduce expression into a Julia expression\n\nExamples\n\njulia> parse(R\"sin(i*x)\")\n:(sinh(x) * im)\n\n\n\n"
},

{
    "location": "index.html#Reduce.rcall",
    "page": "Home",
    "title": "Reduce.rcall",
    "category": "Function",
    "text": "rcall(r::RExpr) Evaluate a Reduce expression.\n\nExamples\n\njulia> R\"int(sin(x), x)\" |> RExpr |> rcall\n - cos(x)\n\n\n\nrcall{T}(expr::T) Evaluate a Julia expression or string using the Reduce interpretor and convert output back into the input type\n\nExamples\n\njulia> rcall(\"int(sin(y)^2, y)\")\n\"( - cos(y)*sin(y) + y)/2\"\njulia> rcall(:(int(1/(1+x^2), x)))\n:(atan(x))\n\n\n\n"
},

{
    "location": "index.html#Functions-1",
    "page": "Home",
    "title": "Functions",
    "category": "section",
    "text": "Reduce.ResetRExprparsercall"
},

{
    "location": "index.html#Index-1",
    "page": "Home",
    "title": "Index",
    "category": "section",
    "text": ""
},

]}
