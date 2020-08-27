global rs = nothing
@info "Precompiling extra Reduce methods (set `ENV[\"REDPRE\"]=\"0\"` to disable)"
Reduce.Load(); atexit(() -> kill(rs))

rcall(:((1+pi)^2)) == convert(Expr,RExpr(rcall("(1+pi)**2")))
try; "1/0" |> rcall; false; catch; true; end
RExpr("(x+i)^3")
Reduce._syme(Reduce.r_to_jl)
R"x+2" == R"2+x-1+1"
:((x+1+π)^2; int(1/(1+x^3),x)) |> RExpr |> Reduce.parse
string(R"x+1")
RExpr(:x) == R"x"
Algebra.:*(RExpr(:x),R"x") == R"x^2"
convert(RExpr,R"x").str == convert(Array{String,1},R"x")
load_package([:rlfi]) == load_package(:rlfi,:rlfi)
rcall(:(x^2+2x+1),off=[:factor])
rcall("x + 1","factor")
Expr(:function,:(fun(z)),:(return begin; x = 700; y = x; end)) |> RExpr |> Reduce.parse
Expr(:for,:(i=2:34),:(product(i))) |> rcall
try; Expr(:type,false,:x) |> RExpr; false; catch; true; end
try; :(@time f(x)) |> RExpr; false; catch; true; end
Expr(:function,:(fun(a,b)),:(return y=a^3+3*a^2*b+3*a*b^2+b^3)) |> factor |> expand
try; Expr(:for,:(i=2:34),:(product(i))) |> RExpr |> parse; false; catch; true; end
R"begin; 1:2; end" |> Reduce.parse |> RExpr |> string
latex(:(x+1))
#Algebra.length(:(x+y))
Algebra.log(:(ℯ^x))
#Algebra.nextprime(100)
#Algebra.ceiling(1.2)
Algebra.impart(:(1+2*im))
#Algebra.impart(2+1.7im)
#Algebra.bernoulli(2)
Sys.islinux() && Reduce.RSymReplace("!#03a9; *x**2 + !#03a9;")
Algebra.int(:(x^2+y),:x) |> RExpr == Algebra.int("x^2+y","x") |> RExpr
R"/(2,begin 2; +(7,4); return +(4,*(2,7))+9 end)" |> Reduce.parse
Algebra.df(Expr(:function,:(fun(z,c)),:(return begin; zn = z^2+c; nz = z^3-1; end))|>RExpr,:z)
:([1 2; 3 4]) |> RExpr |> Reduce.parse |> RExpr == [1 2; 3 4] |> RExpr

Algebra.nextprime("3")
expand("(x-2)^2") |> RExpr == R"(x-2)^2"
nat("x+1")
Algebra.:^(:x,2) == :(x^2)
Algebra.://(NaN,NaN)
join(split(R"x+1;x+2"))
Algebra.sub(:x=>7,Algebra.:+(:x,7)) == Algebra.sub([:x=>7,:z=>21],Algebra.:-(:z,:x))
#squash(Expr(:function,:(fun(x)),:(z=3;z+=:x))).args[2] == squash(:(y=:x;y+=3))
squash(:(sqrt(x)^2))
Expr(:block,:(x+1)) |> RExpr == R"1+x"
Algebra.limit(Algebra.:^(Algebra.:-(1,Algebra.:/(1,:n)),Algebra.:-(:n)),:n,Inf)
Algebra.log(Algebra.exp(:pi))
Algebra.://(2,Inf)
Algebra.://(Inf,2)

rcall("x")
Algebra.operator(:x); Algebra.clear(:x);
#try Algebra.det([:x :y]) catch; true end
join([R"1",R"1"]) == R"1;1"
list([R"1",R"x"]) == list((1,:x))
Reduce.lister(:x) == R"x"
!latex(false)
@rounded @factor x^2-2x+1
@rounded @off_factor @rcall x^2
Algebra.det([:a :b; :c :d])
Algebra.tp([:a;:b]) == [:a :b;]
transpose(:x)
adjoint(:x)
Algebra.operator(:cbrt)
Algebra.rlet(:(cbrt(~x))=>:(x^(1/3)))
Algebra.rlet([:(cbrt(~x))=>:(x^(1/3))])
Algebra.rlet(Dict(:(cbrt(~x))=>:(x^(1/3))))
Algebra.clear(:cbrt)
Algebra.:+(1,R"x")
Algebra.inv([:a :b; :c :d])
Algebra.inv(1)
Algebra.:\([1],[2])
#Algebra.://(1.0,1.0)
Algebra.:/([:a :b; :c :d],2)
Algebra.:+([:a :b; :c :d],1) == Algebra.:+(1,[:a :b; :c :d])
Algebra.:+([:x],1) == Algebra.:+(1,[:x])
Algebra.:+([:x,:y]',1) == Algebra.:+(1,[:x,:y]')
Algebra.solve(:(x-1),:x) == Algebra.solve((:(x-1),),:x)
Algebra.order(nothing); Algebra.korder(nothing)
