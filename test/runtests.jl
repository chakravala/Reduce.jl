using Reduce
using Base.Test

# write your own tests here
@test rcall(:((1+π)^2)) == convert(Expr,RExpr(rcall("(1+pi)**2")))
@test try; "1/0" |> rcall; false; catch; true; end
@test ResetReduce() == nothing
@test display(RExpr("(x+1)^3")) == nothing
@test Reduce._syme(Reduce.r_to_jl) |> typeof == String
@test ra"x+2" == ra"2+x-1+1"
@test :((x+1+π)^2; int(1/(1+x^3),x)) |> RExpr |> parse |> typeof == Expr
