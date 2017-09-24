using Reduce
using Base.Test

rounded(true)

# write your own tests here
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
#@test Reduce.repl_init(Base.active_repl)==nothing
