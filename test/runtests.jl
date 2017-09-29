using Reduce
using Base.Test

# write your own tests here
@test showerror(STDOUT,ReduceError("A Portable General-Purpose Computer Algebra System")) == nothing
@test rcall(:((1+pi)^2)) == convert(Expr,RExpr(rcall("(1+pi)**2")))
@test try; "1/0" |> rcall; false; catch; true; end
@test Reduce.Reset() == nothing
@test display(RExpr("(x+i)^3")) == nothing; print('\n')
@test Reduce._syme(Reduce.r_to_jl) |> typeof == String
@test R"x+2" == R"2+x-1+1"
@test :((x+1+π)^2; int(1/(1+x^3),x)) |> RExpr |> parse |> typeof == Expr
@test !Base.process_exited(Reduce.rs)
@test string(R"x+1") |> typeof == String
@test RExpr(:x) == R"x"
@test RExpr(:x)*R"x" == R"x;x"
@test convert(RExpr,R"x").str == convert(Array{Compat.String,1},R"x")
@test load_package([:rlfi]) == nothing
@test show(STDOUT, R"") == nothing
@test show(STDOUT,"text/latex",R"int(sinh(e**i*z),z)") == nothing
@test Base.write(Reduce.rs, R"") |> typeof == Int
@test "1"*R"1"*"1" == R"1;1;1"
@test (x = :(x^2+2x+1); rcall(x,off=[:factor]) == x)
@test Expr(:function,:fun,:(return begin; y = x; end)) |> RExpr |> parse |> typeof == Expr
@test Expr(:for,:(i=2:34),:(product(i))) |> rcall |> eval |> typeof == BigInt
@test try; Expr(:type,false,:x) |> RExpr; false; catch; true; end
@test try; :(@time f(x)) |> RExpr; false; catch; true; end
@test (x = Expr(:function,:fun,:(return y=a^3+3*a^2*b+3*a*b^2+b^3)); x==x |> Reduce.factor |> expand)
@test try; Expr(:for,:(i=2:34),:(product(i))) |> RExpr |> parse; false; catch; true; end
@test R"begin; 1:2; end" |> parse |> RExpr |> string == "1:2 "
@test latex(:(x+1)) |> typeof == String
@test length(:(x+y)) |> typeof == Int
@test log(:(e^x)) == :x
@test nextprime(100) == 101
@test ceiling(1.2) == 2
@test impart(:(1+2*im)) == 2
@test bernoulli(2) == 1//6
println()
#@test Reduce.repl_init(Base.active_repl)==nothing
