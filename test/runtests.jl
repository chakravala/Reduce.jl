using Reduce, Compat
using Base.Test
@force using Reduce.Algebra
is_windows() && (Reduce.ColCheck(false); * = Algebra.:*)

# write your own tests here
@test showerror(STDOUT,ReduceError("A Portable General-Purpose Computer Algebra System")) == nothing
@test rcall(:((1+pi)^2)) == convert(Expr,RExpr(rcall("(1+pi)**2")))
@test try; "1/0" |> rcall; false; catch; true; end
@test Reduce.Reset() == nothing
@test display(RExpr("(x+i)^3")) == nothing; print('\n')
@test Reduce._syme(Reduce.r_to_jl) |> typeof == String
@test R"x+2" == R"2+x-1+1"
@test :((x+1+π)^2; int(1/(1+x^3),x)) |> RExpr |> Reduce.parse |> typeof == Expr
@test !Base.process_exited(Reduce.rs)
@test string(R"x+1") |> typeof == String
@test RExpr(:x) == R"x"
@test RExpr(:x)*R"x" == R"x^2"
@test convert(RExpr,R"x").str == convert(Array{Compat.String,1},R"x")
@test load_package([:rlfi]) == load_package(:rlfi,:rlfi)
@test show(STDOUT, R"") == nothing
@test show(STDOUT,"text/latex",R"int(sinh(e**i*z),z)") == nothing
@test Base.write(Reduce.rs, R"") |> typeof == Int; rcall(1)
#@test "1"*R"1"*"1" == split(R"1;1;1")
@test (x = :(x^2+2x+1); rcall(x,off=[:factor]) == x)
@test rcall("x + 1","factor") == "x + 1"
@test Expr(:function,:fun,:(return begin; x = 700; y = x; end)) |> RExpr |> Reduce.parse |> typeof == Expr
@test Expr(:for,:(i=2:34),:(product(i))) |> rcall |> eval |> typeof == BigInt
@test try; Expr(:type,false,:x) |> RExpr; false; catch; true; end
@test try; :(@time f(x)) |> RExpr; false; catch; true; end
@test (x = Expr(:function,:fun,:(return y=a^3+3*a^2*b+3*a*b^2+b^3)); x==x |> Reduce.factor |> expand)
@test try; Expr(:for,:(i=2:34),:(product(i))) |> RExpr |> Reduce.parse; false; catch; true; end
@test R"begin; 1:2; end" |> Reduce.parse |> RExpr |> string == "1:2 "
@test latex(:(x+1)) |> typeof == String
@test length(:(x+y)) |> typeof == Int
@test log(:(e^x)) == :x
@test nextprime(100) == 101
@test ceiling(1.2) == 2
@test impart(:(1+2*im)) == 2
@test impart(2+1.7im) == 17//10
@test bernoulli(2) == 1//6
@test Reduce.parsegen(:parsetest,:expr) |> typeof == Expr
@test Reduce.parsegen(:calctest,:args) |> typeof == Expr
@test Reduce.parsegen(:switchtest,:switch) |> typeof == Expr
@test Reduce.parsegen(:unarytest,:unary) |> typeof == Expr
!(VERSION < v"0.6.0") && is_linux() && @test Reduce.RSymReplace("!#03a9; *x**2 + !#03a9;") |> typeof == String
@test :((x+im+π)^2; int(1/(1+x^3),x)) |> RExpr |> rcall |> Reduce.parse |> typeof == Expr
@test :(int(sin(im*x+pi)^2-1,x)) |> rcall |> typeof == Expr
@test int(:(x^2+y),:x) |> RExpr == int("x^2+y","x") |> RExpr
@test R"/(2,begin 2; +(7,4); return +(4,*(2,7))+9 end)" |> Reduce.parse |> typeof == Expr
@test df(Expr(:function,:fun,:(return begin; zn = z^2+c; nz = z^3-1; end))|>RExpr,:z) |> typeof == RExpr
@test :([1 2; 3 4]) |> RExpr |> Reduce.parse |> RExpr == [1 2; 3 4] |> RExpr
println()
#@test Reduce.repl_init(Base.active_repl)==nothing

@test nextprime("3") == "5"
@test expand("(x-2)^2") |> RExpr == R"(x-2)^2"
@test nat("x+1") == "\nx + 1\n"
#@test macroexpand(@factor(:(x^2+2x+1))) == :((x+1)^2)
@test :x^2 == :(x^2)
@test NaN//NaN |> isnan
@test join(split(R"x+1;x+2"))|> string == "x+1;\nx+2"
@test sub(:x=>7,:x+7) == sub([:x=>7,:z=>21],:z-:x)
@test sub(Float64,prod((:x-:n)^:n,:n,1,7)|>horner) |> typeof == Expr
@test squash(Expr(:function,:(fun(x)),:(z=3;z+=:x))).args[2] == squash(:(y=:x;y+=3))
@test Expr(:block,:(x+1)) |> RExpr == R"1+x"
@test limit((1-1/:n)^-:n,:n,Inf) == e
@test log(exp(:pi)) == π
@test 2//Inf == 0
@test Inf//2 == Inf
