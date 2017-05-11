using Reduce
using Base.Test

# write your own tests here
@test rcall(:((1+π)^2)) == convert(Expr,RExpr(rcall("(1+pi)**2")))
@test try; "1/0" |> rcall; false; catch; true; end
