preload = false
try
    (ENV["REDPRE"] == "1") && (preload = true)
end
if preload
    info("Compiling Reduce parser functions.")
    Load(); atexit(() -> kill(rs))
    rcall(:((1+pi)^2)) == convert(Expr,RExpr(rcall("(1+pi)**2")))
    try; "1/0" |> rcall; false; catch; true; end
    RExpr("(x+i)^3")
    Reduce._syme(Reduce.r_to_jl) |> typeof == String
    R"x+2" == R"2+x-1+1"
    :((x+1+π)^2; int(1/(1+x^3),x)) |> RExpr |> Reduce.parse |> typeof == Expr
    string(R"x+1") |> typeof == String
    RExpr(:x) == R"x"
    RExpr(:x)*R"x" == R"x;x"
    convert(RExpr,R"x").str == convert(Array{Compat.String,1},R"x")
    load_package([:rlfi]) == load_package(:rlfi,:rlfi)
    "1"*R"1"*"1" == R"1;1;1"
    (x = :(x^2+2x+1); rcall(x,off=[:factor]) == x)
    rcall("x + 1","factor") == "x + 1"
    Expr(:function,:fun,:(return begin; x = 700; y = x; end)) |> RExpr |> Reduce.parse |> typeof == Expr
    Expr(:for,:(i=2:34),:(product(i))) |> rcall |> eval |> typeof == BigInt
    try; Expr(:type,false,:x) |> RExpr; false; catch; true; end
    try; :(@time f(x)) |> RExpr; false; catch; true; end
    (x = Expr(:function,:fun,:(return y=a^3+3*a^2*b+3*a*b^2+b^3)); x==x |> Reduce.factor |> expand)
    try; Expr(:for,:(i=2:34),:(product(i))) |> RExpr |> Reduce.parse; false; catch; true; end
    R"begin; 1:2; end" |> Reduce.parse |> RExpr |> string == "1:2 "
    latex(:(x+1)) |> typeof == String
    length(:(x+y)) |> typeof == Int
    log(:(e^x)) == :x
    nextprime(100) == 101
    ceiling(1.2) == 2
    impart(:(1+2*im)) == 2
    impart(2+1.7im) == 17//10
    bernoulli(2) == 1//6
    !(VERSION < v"0.6.0") && is_linux() && Reduce.RSymReplace("!#03a9; *x**2 + !#03a9;") |> typeof == String
    int(:(x^2+y),:x) |> RExpr == int("x^2+y","x") |> RExpr
    R"/(2,begin 2; +(7,4); return +(4,*(2,7))+9 end)" |> Reduce.parse |> typeof == Expr
    df(Expr(:function,:fun,:(return begin; zn = z^2+c; nz = z^3-1; end))|>RExpr,:z) |> typeof == RExpr
    :([1 2; 3 4]) |> RExpr |> Reduce.parse |> RExpr == [1 2; 3 4] |> RExpr
end
