#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

const calculus = [
    :df,
    :int,
    :limit,
    :logb,
    :solve,
    :pf,
    :structr,
    :coeff,
    :coeffn,
    :part,
    :factorize,
    :remainder,
    :resultant,
    :interpol,
    :deg,
    :lcof,
    :lpower,
    :lterm,
    :reduct,
    :totaldeg,
    :pochhammer,
    :fibonaccip,
    :factor,
    :remfac
]

const cnan = [
    :clear,
    :matrix,
    :operator,
    :listargp,
    :infix,
    :precedence,
    :depend,
    :nodepend,
    :realvalued,
    :notrealvalued,
    :set,
    :unset,
    :mkid,
    :even,
    :odd,
    :linear,
    :noncom,
    :symmetric,
    :antisymmetric,
    :order,
    :korder,
    :on,
    :off,
]

const alg = [
    :sum,
    :prod,
    :max,
    :min,
]

const iops = [
    :+,
    :-,
    :*,
    :^,
    :/,
    ://
]

const cmat = [
    #:mateigen,
    #:cofactor
]

Expr(:block,[:($i(r...)=Base.$i(r...)) for i ∈ [alg;iops]]...) |> eval
#Expr(:toplevel,[:(import Base: $i) for i ∈ [alg;iops]]...) |> eval
:(export $([calculus;cnan;alg;iops;cmat]...)) |> eval
#:(export $(Symbol.("@",[calculus;alg;iops])...)) |> eval

for fun in [calculus;alg;iops]
    @eval begin
        $(Reduce.parsegen(fun,:args))
        $(Reduce.unfoldgen(fun,:args))
        #=macro $fun(expr,s...)
            :($$(QuoteNode(fun))($(esc(expr)),$(esc(s))...))
        end=#
    end
end

for fun in [calculus;alg]
    @eval begin
        function $fun(expr::Compat.String,s...;be=0)
            convert(Compat.String, $fun(RExpr(expr),s...;be=be))
        end
    end
end

for fun in cnan
    @eval begin
        $fun(r::RExpr...) = string($(string(fun)),"(",join(string.(r),","),")") |> rcall |> RExpr
        $fun(r...) = $fun(RExpr.(r)...) |> parse
    end
end

for fun in cmat
    @eval begin
        $(Reduce.parsegen(fun,:args))
        function $fun(expr::Union{Array{Any,2},Expr,Symbol},s...)
            $fun(RExpr(expr),RExpr.(s)...)
        end
        function $fun(expr::Union{Array{T,2},Expr,Symbol},s...) where T <: ExprSymbol
            $fun(RExpr(expr),RExpr.(s)...)
        end
    end
end

for fun in iops
    @eval begin
        function $fun(a::Union{<:Number,Expr,Symbol},r::RExpr,s...)
            $fun(RExpr(a),r,RExpr.(s)...) |> parse
        end
        function $fun(a::T,b::ExprSymbol,s...) where T <: Number
            $fun(RExpr(a),RExpr(b),RExpr.(s)...) |> parse
        end
    end
end

const MatExpr = Union{Array{Any,2},Array{Expr,2},Array{Symbol,2},Expr,Symbol,<:Number}
const Mat = Union{Vector,RowVector,Array{Any,2},Array{Expr,2},Array{Symbol,2}}
const MatOnly = Union{Array{Any,2},Array{Expr,2},Array{Symbol,2}}
const ESN = Union{Expr,Symbol,<:Number}

^(expr::Array{Any,2},s::Integer) = ^(RExpr(expr),s) |> parse |> mat
^(expr::Array{T,2},s::Integer) where T <: ExprSymbol = ^(RExpr(expr),s) |> parse |> mat
^(expr::RExpr,s::Integer) = ^(expr,RExpr(s))
^(a::T,s::ExprSymbol) where T <: Number = ^(RExpr(a),RExpr(s)) |> parse

*(a::T,s::S) where T <: MatOnly where S <: ESN = *(RExpr(a),RExpr(s)) |> parse |> mat
*(a::T,s::S) where T <: ESN where S <: MatOnly = *(RExpr(a),RExpr(s)) |> parse |> mat
*(a::T,s::S) where T <: MatOnly where S <: MatOnly = *(RExpr(a),RExpr(s)) |> parse |> mat
*(a::T,s::S) where T <: Vector where S <: RowVector = *(RExpr(a),RExpr(s)) |> parse |> mat
*(a::T,s::S) where T <: Vector where S <: MatExpr = *(RExpr(a),RExpr(s)) |> parse |> mat
*(a::T,s::S) where T <: RowVector where S <: Vector = *(RExpr(a),RExpr(s)) |> parse |> mat
*(a::T,s::S) where T <: RowVector where S <: MatExpr = *(RExpr(a),RExpr(s)) |> parse |> mat
*(a::T,s::S) where T <: MatExpr where S <: Vector = *(RExpr(a),RExpr(s)) |> parse |> mat
*(a::T,s::S) where T <: MatExpr where S <: RowVector = *(RExpr(a),RExpr(s)) |> parse |> mat

for o in [:+,:-]
    @eval begin
        function $o(a::T,s::S) where T <: MatOnly where S <: ESN
            $o(RExpr(a),RExpr(s)*RExpr(ones(size(a)))) |> parse |> mat
        end
        function $o(a::T,s::S) where T <: ESN where S <: MatOnly
            $o(RExpr(a)*RExpr(ones(size(s))),RExpr(s)) |> parse |> mat
        end
        $o(a::T,s::S) where T <: MatOnly where S <: MatOnly = $o(RExpr(a),RExpr(s)) |> parse |> mat
        $o(a::T,s::S) where T <: Vector where S <: RowVector = $o(RExpr(a),RExpr(s)) |> parse |> mat
        $o(a::T,s::S) where T <: Vector where S <: MatOnly = $o(RExpr(a),RExpr(s)) |> parse |> mat
        function $o(a::T,s::ESN) where T <: Vector
            $o(RExpr(a),RExpr(s)*RExpr(ones(size(a)))) |> parse |> mat
        end
        $o(a::T,s::S) where T <: RowVector where S <: Vector = $o(RExpr(a),RExpr(s)) |> parse |> mat
        $o(a::T,s::S) where T <: RowVector where S <: MatOnly = $o(RExpr(a),RExpr(s)) |> parse |> mat
        function $o(a::T,s::ESN) where T <: RowVector
            $o(RExpr(a),RExpr(s)*RExpr(ones(size(a)))) |> parse |> mat
        end
        $o(a::T,s::S) where T <: MatOnly where S <: Vector = $o(RExpr(a),RExpr(s)) |> parse |> mat
        function $o(a::ESN,s::S) where S <: Vector
            $o(RExpr(a)*RExpr(ones(size(s))),RExpr(s)) |> parse |> mat
        end
        $o(a::T,s::S) where T <: MatOnly where S <: RowVector = $o(RExpr(a),RExpr(s)) |> parse |> mat
        function $o(a::ESN,s::S) where S <: RowVector
            $o(RExpr(a)*RExpr(ones(size(s))),RExpr(s)) |> parse |> mat
        end
    end
end

for o in [:/,://]
    @eval begin
        $o(a::MatOnly,b::ESN) = $o(RExpr(a),RExpr(b)) |> parse |> mat
    end
end
//(expr,b::T) where T <: AbstractFloat = //(RExpr(expr),RExpr(b)) |> parse
//(a::T,expr) where T <: AbstractFloat = //(RExpr(a),RExpr(expr)) |> parse
//(expr::ExprSymbol,b::T) where T <: AbstractFloat = //(RExpr(expr),RExpr(b)) |> parse
//(a::T,expr::ExprSymbol) where T <: AbstractFloat = //(RExpr(a),RExpr(expr)) |> parse
function //(a::T,b::T) where T <: AbstractFloat
    isnan(a) | isnan(b) | (isinf(a) & isinf(b)) && return NaN
    return //(RExpr(a),RExpr(b)) |> parse |> eval
end

export inv, \

inv(r) = Base.inv(r)
inv(r::RExpr) = r^-1
inv(r::T) where T <: MatExpr = r^-1

\(a,b) = Base.:\(a,b)
\(a::T,s::S) where T <: MatExpr where S <: Vector = (RExpr(a)^-1)*RExpr(s) |> parse |> mat
\(a::T,s::S) where T <: Vector where S <: MatExpr = (RExpr(a)^-1)*RExpr(s) |> parse |> mat

function solve(a::Array{T,1},s::Array{Symbol,1}) where T <: Any
    out = solve(list(a),list(s))
    return T <: String ? out.str : parse(out)
end
solve(a::T,s::Symbol) where T <: Vector = solve(a,[s])
solve(a::Expr,s::Array{Symbol,1}) = solve(a.head == :block ? a.args : [a],s)
solve(a::Expr,s::Symbol) = solve(a,[s])
solve(a::T,s::S) where T <: Tuple where S <: Tuple = solve(RExpr(a),RExpr(s)) |> parse
solve(a::T,s::Symbol) where T <: Tuple = solve(a,(s,))

order(::Void) = order(R"nil") |> parse
korder(::Void) = korder(R"nil") |> parse

export ∑, ∏, sub

(∑, ∏) = (sum, prod)

"""
    sub(::Union{Dict,Pair},expr)

Make variable substitutions using Reduce's native sub command. Syntax:
```
R"(⟨substitution_list⟩,⟨EXPRN1:algebraic⟩)"
```
where `⟨substitution_list⟩` is a list of one or more equations of the form
```
⟨VAR:kernel⟩ = ⟨EXPRN:algebraic⟩
```
or a kernel that evaluates to such a list.

The `sub` operator gives the algebraic result of replacing every occurrence of the variable `var` in the expression `EXPRN1` by the expression `EXPRN`. Specifically, `EXPRN1` is first evaluated using all available rules. Next the substitutions are made, and finally the substituted expression is reevaluated. When more than one variable occurs in the substitution list, the substitution is performed by recursively walking down the tree representing `EXPRN1`, and replacing every `VAR` found by the appropriate `EXPRN`. The `EXPRN` are not themselves searched for any occurrences of the various `VAR`s. The trivial case `sub`(EXPRN1)` returns the algebraic value of `EXPRN1`.

*Examples:*
```
                                    2              2
     sub({x=a+y,y=y+1},x^2+y^2) -> A  + 2*A*Y + 2*Y  + 2*Y + 1
```
and with `R"s := {x=a+y,y=y+1}"`,
```
                                    2              2
     sub(s,x^2+y^2)             -> A  + 2*A*Y + 2*Y  + 2*Y + 1
```
Note that the global assignments `R"x:=a+y"`, etc., do not take place.

`EXPRN1` can be any valid algebraic expression whose type is such that a substitution process is defined for it (e.g., scalar expressions, lists and matrices). An error will occur if an expression of an invalid type for substitution occurs either in `EXPRN` or `EXPRN1`.
"""
sub(syme::String,expr::RExpr) = "sub($syme,$expr)" |> rcall |> RExpr
sub(syme::String,expr::T) where T = convert(T,sub(syme,RExpr(expr)))
sub(s::Dict{String,String},expr) = sub(Reduce._syme(s),expr)
sub(s::Dict{<:Any,<:Any},expr) = sub(Dict([=>(string.(RExpr.([b[1],b[2]]))...) for b ∈ collect(s)]...),expr)
sub(s::Pair{<:Any,<:Any},expr) = sub(Dict(s),expr)
sub(s::Array{<:Pair{<:Any,<:Any},1},expr) = sub(Dict(s...),expr)

"""
    sub(T::DataType,expr::Expr)

Make a substitution to convert numerical values to type T
"""
function sub(T::DataType,ixpr)
    if typeof(ixpr) == Expr
        expr = deepcopy(ixpr)
        if expr.head == :call && expr.args[1] == :^
            expr.args[2] = sub(T,expr.args[2])
            if typeof(expr.args[3]) == Expr
                expr.args[3] = sub(T,expr.args[3])
            end
        elseif expr.head == :macrocall &&
                expr.args[1] ∈ [Symbol("@int128_str"), Symbol("@big_str")]
            return convert(T,eval(expr))
        else
            for a ∈ 1:length(expr.args)
                expr.args[a] = sub(T,expr.args[a])
            end
        end
        return expr
    elseif typeof(ixpr) <: Number
        return convert(T,ixpr)
    end
    return ixpr
end

@doc """
    on(::Symbol...)

Takes a list of switch names as argument and turns them on.
""" Reduce.Algebra.on

@doc """
    off(::Symbol...)

Takes a list of switch names as argument and turns them off.
""" Reduce.Algebra.off

@doc """
    max(r...)

`max` can take an arbitrary number of expressions as their arguments. If all arguments evaluate to numerical values, the maximum of the argument list is returned. If any argument is non-numeric, an appropriately reduced expression is returned. For example:

```Julia
julia> Algebra.max(2,-3,4,5)
5

julia> Algebra.max(:a,2,3)
:(max(3, a))
```

`max` of an empty list returns 0.
""" Reduce.Algebra.max

@doc """
    min(r...)

`min` can take an arbitrary number of expressions as their arguments. If all arguments evaluate to numerical values, the minimum of the argument list is returned. If any argument is non-numeric, an appropriately reduced expression is returned. For example:

```Julia
julia> Algebra.min(2,-2)
-2

julia> Algebra.min(:x)
:x
```

`min` of an empty list returns 0.
""" Reduce.Algebra.min

@doc """
    df(r...)

The operator `df` is used to represent partial differentiation with respect to one or more variables. It is used with the syntax:
```Julia
R"df(⟨EXPRN:algebraic⟩[,⟨VAR:kernel⟩ <,⟨NUM:integer⟩ >])"
```
The first argument is the expression to be differentiated. The remaining arguments specify the differentiation variables and the number of times they are applied.

The number `num` may be omitted if it is `1`. For example,
```Julia
reduce> df(y,x)

reduce> df(y,x,2)

reduce> df(y,x1,2,x2,x3,2)
```
The evaluation of `df(y,x)` proceeds as follows: first, the values of `y` and `x` are found. Let us assume that `x` has no assigned value, so its value is `x`. Each term or other part of the value of `y` that contains the variable `x` is differentiated by the standard rules. If `z` is another variable, not `x` itself, then its derivative with respect to `x` is taken to be `0`, unless `z` has previously been declared to `depend` on `x`, in which case the derivative is reported as the symbol `df(z,x)`.
""" Reduce.Algebra.df

@doc """
    int(r...)

`int` is an operator in REDUCE for indefinite integration using a combination of the Risch-Norman algorithm and pattern matching. It is used with the syntax:
```Julia
R"int(⟨EXPRN:algebraic⟩,⟨VAR:kernel⟩)"
```
This will return correctly the indefinite integral for expressions comprising polynomials, log functions, exponential functions and tan and atan. The arbitrary constant is not represented. If the integral cannot be done in closed terms, it returns a formal integral for the answer in one of two ways:

1. It returns the input, `int(…,…)` unchanged.
2. It returns an expression involving `int`s of some other functions (sometimes more complicated than the original one, unfortunately).

Rational functions can be integrated when the denominator is factorizable by the program. In addition it will attempt to integrate expressions involving error functions, dilogarithms and other trigonometric expressions. In these cases it might not always succeed in finding the solution, even if one exists.

*Examples:*
```Julia
julia> Algebra.int(:(log(x)),:x)
:((log(x) - 1) * x)

julia> Algebra.int(:(e^x),:x)
:(e ^ x)
```
The program checks that the second argument is a variable and gives an error if it is not.

*Note:* If the `int` operator is called with 4 arguments, REDUCE will implicitly call the definite integration package (DEFINT) and this package will interpret the third and fourth arguments as the lower and upper limit of integration, respectively. For details, consult the documentation on the DEFINT package.
""" Reduce.Algebra.int

@doc """
    pochhammer(a,k)

The Pochhammer notation ``(a)_k`` (also called Pochhammer’s symbol) is supported by the binary operator `pochhammer(a,k)`. For a non-negative integer `k`, it is defined as ([http://dlmf.nist.gov/5.2.iii](http://dlmf.nist.gov/5.2.iii))

``(a)_0	= 1,``

``(a)_k	= a(a + 1)(a + 2)⋅⋅⋅(a + k - 1).``

For ``a ⁄= 0,±1,±2,…``, this is equivalent to

``(a)k	= \\frac{\\Gamma (a+-k-)}{\\Gamma (a)}``

With `rounded` off, this expression is evaluated numerically if `a` and `k` are both integral, and otherwise may be simplified where appropriate. The simplification rules are based upon algorithms supplied by Wolfram Koepf.
""" Reduce.Algebra.pochhammer

@doc """
    pf(expr,var)

`R"pf(⟨exp⟩,⟨var⟩)"` transforms the expression `⟨exp⟩` into a list of partial fractions with respect to the main variable, `⟨var⟩`. `pf` does a complete partial fraction decomposition, and as the algorithms used are fairly unsophisticated (factorization and the extended Euclidean algorithm), the code may be unacceptably slow in complicated cases.
""" Reduce.Algebra.pf

@doc """
    solve(r...)

`solve` is an operator for solving one or more simultaneous algebraic equations. It is used with the syntax:
```Julia
R"SOLVE(⟨EXPRN:algebraic⟩[,⟨VAR:kernel⟩∣,⟨VARLIST:list of kernels⟩])"
```
`exprn` is of the form `⟨expression⟩` or `{⟨expression1⟩,⟨expression2⟩, …}`. Each expression is an algebraic equation, or is the difference of the two sides of the equation. The second argument is either a kernel or a list of kernels representing the unknowns in the system. This argument may be omitted if the number of distinct, non-constant, top-level kernels equals the number of unknowns, in which case these kernels are presumed to be the unknowns.

For one equation, `solve` recursively uses factorization and decomposition, together with the known inverses of `log`, `sin`, `cos`, `^`, `acos`, `asin`, and linear, quadratic, cubic, quartic, or binomial factors. Solutions of equations built with exponentials or logarithms are often expressed in terms of Lambert’s `W` function. This function is (partially) implemented in the special functions package.

Linear equations are solved by the multi-step elimination method due to Bareiss, unless the switch `cramer` is on, in which case Cramer’s method is used. The Bareiss method is usually more efficient unless the system is large and dense.

Non-linear equations are solved using the Groebner basis package (chapter 16.28). Users should note that this can be quite a time consuming process.

*Examples:*
```Julia
Algebra.solve(:(log(sin(x+3))^5 == 8),:x)
Algebra.solve(:(a*log(sin(x+3))^5 - b), :(sin(x+3)))
Algebra.solve((:(a*x+y==3),:(y=-2)),(:x,:y))
```
`solve` returns a list of solutions. If there is one unknown, each solution is an equation for the unknown. If a complete solution was found, the unknown will appear by itself on the left-hand side of the equation. On the other hand, if the solve package could not find a solution, the “solution” will be an equation for the unknown in terms of the operator `root_of`. If there are several unknowns, each solution will be a list of equations for the unknowns. For example,
```Julia
julia> Algebra.solve(:(x^2==1),:x)
(:(x = 1), :(x = -1))

julia> Algebra.solve(:(x^7-x^6+x^2==1),:x)
(:(x = root_of(x_ ^ 6 + x_ + 1, x_, tag_1)), :(x = 1))

julia> Algebra.solve((:(x+3y==7),:(y-x==1)),(:x,:y))
(:(x = 1), :(y = 2))
```
The `tag` argument is used to uniquely identify those particular solutions.
```
""" Reduce.Algebra.solve

@doc """
    even(r...)

An operator can be declared to be even in its first argument by the declarations `even`. Expressions involving an operator declared in this manner are transformed if the first argument contains a minus sign. Any other arguments are not affected. For example, the declaration
```Julia
julia> Algebra.even(:f1)
```
means that
```
        f1(-a)    ->    f1(a)  
        f1(-a,-b) ->    f1(a,-b)  
```
""" Reduce.Algebra.even

@doc """
    odd(r...)

An operator can be declared to be odd in its first argument by the declarations `odd`. Expressions involving an operator declared in this manner are transformed if the first argument contains a minus sign. Any other arguments are not affected. In addition, if say `f` is declared odd, then `f(0)` is replaced by zero unless `f` is also declared non zero by the declaration `nonzero`. For example, the declarations
```Julia
julia> Algebra.odd(:f2)
```
means that
```
        f2(-a)    ->   -f2(a)  
        f2(0)     ->    0
```
To inhibit the last transformation, say `nonzero(:f2)`.
""" Reduce.Algebra.odd

@doc """
    linear(r...)

An operator can be declared to be linear in its first argument over powers of its second argument. If an operator `f` is so declared, `f` of any sum is broken up into sums of `f`s, and any factors that are not powers of the variable are taken outside. This means that `f` must have (at least) two arguments. In addition, the second argument must be an identifier (or more generally a kernel), not an expression.

*Example:* If `f` were declared linear, then
```
f(a*x^5+b*x+c,x) ->  f(x^5,x)*a + f(x,x)*b + f(1,x)*c
```
More precisely, not only will the variable and its powers remain within the scope of the `f` operator, but so will any variable and its powers that had been declared to `depend` on the prescribed variable; and so would any expression that contains that variable or a dependent variable on any level, e.g. `cos(sin(x))`.

To declare operators `f` and `g` to be linear operators, use:
```Julia
julia> Algebra.linear(:f,:g)
```
The analysis is done of the first argument with respect to the second; any other arguments are ignored. It uses the following rules of evaluation:
```
f(0) 		-> 0
f(-y,x) 	-> -f(y,x)
f(y+z,x) 	-> f(y,x)+f(z,x)
f(y*z,x) 	-> z*f(y,x)   	if z does not depend on x
f(y/z,x) 	-> f(y,x)/z	if z does not depend on x
```
""" Reduce.Algebra.linear

@doc """
    noncom(r...)

An operator can be declared to be non-commutative under multiplication by the declaration `noncom`.

*Example:* After the declaration
```Julia
julia> Algebra.noncom(:u,:v);
```
the expressions `u(x)*u(y)-u(y)*u(x)` and `u(x)*v(y)-v(y)*u(x)` will remain unchanged on simplification, and in particular will not simplify to zero.

Note that it is the operator (`u` and `v` in the above example) and not the variable that has the non-commutative property.
""" Reduce.Algebra.noncom


@doc """
    symmetric(r...)

An operator can be declared to be symmetric with respect to its arguments by the declaration `symmetric`. For example
```Julia
julia> Algebra.symmetric(:u,:v);
```
means that any expression involving the top level operators `u` or `v` will have its arguments reordered to conform to the internal order used by REDUCE. The user can change this order for kernels by the command `korder`.
For example, `u(x,v(1,2))` would become `u(v(2,1),x)`, since numbers are ordered in decreasing order, and expressions are ordered in decreasing order of complexity.
""" Reduce.Algebra.symmetric

@doc """
    antisymmetric(r...)

the declaration `antisymmetric` declares an operator antisymmetric. For example,
```Julia
julia> Algebra.antisymmetric(:l,:m);
```
means that any expression involving the top level operators `l` or `m` will have its arguments reordered to conform to the internal order of the system, and the sign of the expression changed if there are an odd number of argument interchanges necessary to bring about the new order.

For example, `l(x,m(1,2))` would become `-l(-m(2,1),x)` since one interchange occurs with each operator. An expression like `l(x,x)` would also be replaced by `0`.
""" Reduce.Algebra.antisymmetric

@doc """
    operator(r...)

The user may add new prefix operators to the system by using the declaration `operator`. For example:
```Julia
julia> Algebra.operator(:h,:g1,:arctan)
```
adds the prefix operators `h`, `g1` and `arctan` to the system.

This allows symbols like `h(w)`, `h(x,y,z)`, `g1(p+q)`, `arctan(u/v)` to be used in expressions, but no meaning or properties of the operator are implied. The same operator symbol can be used equally well as a 0-, 1-, 2-, 3-, etc.-place operator.

To give a meaning to an operator symbol, or express some of its properties, `let` statements can be used, or the operator can be given a definition as a procedure.
""" Reduce.Algebra.operator

@doc """
    depend(r...)

There are several facilities in REDUCE, such as the differentiation operator and the linear operator facility, that can utilize knowledge of the dependency between various variables, or kernels. Such dependency may be expressed by the command `depend`. This takes an arbitrary number of arguments and sets up a dependency of the first argument on the remaining arguments. For example,
```Julia
julia> Algebra.depend(:x,:y,:z)
```
says that `x` is dependent on both `y` and `z`.
```Julia
julia> Algebra.depend(:z,:(cos(x)),:y)
```
says that `z` is dependent on `cos(x)` and `y`.
""" Reduce.Algebra.depend

@doc """
    nodepend(r...)

Dependencies introduced by `depend` can be removed by `nodepend`. The arguments of this are the same as for `depend`. For example, given the above dependencies,
```Julia
julia> Algebra.nodepend(:z,:(cos(x)))
```
says that `z` is no longer dependent on `cos(x)`, although it remains dependent on `y`.
""" Reduce.Algebra.nodepend

@doc """
    set(a,b)

In some cases, it is desirable to perform an assignment in which both the left- and right-hand sides of an assignment are evaluated. In this case, the `set` statement can be used with the syntax:
```Julia
R"set(⟨expression⟩,⟨expression⟩)"
```
For example, the statements
```
        j := 23;  
        set(mkid(a,j),x);
```
assigns the value `x` to `a23`.
""" Reduce.Algebra.set

@doc """
    unset(r)

To remove a value from such a variable, the `unset` statement can be used with the syntax:
```
R"unset(⟨expression⟩)"
```
For example, the statement
```
        j := 23;  
        unset(mkid(a,j));
```
clears the value of `a23`.
""" Reduce.Algebra.unset

@doc """
    fibonacci(n,x)

Fibonacci Polynomials are computed by the binary operator `fibonaccip`. `fibonaccip(n,x)` returns the `n`th Fibonacci polynomial in the variable `x`. If `n` is a positive or negative integer, it will be evaluated following the definition:

\$F_0(x) = 0; F_1(x) = 1; F_n(x) = xF_{n-1}(x) + F_{n-2}(x)\$
""" Reduce.Algebra.fibonaccip

@doc """
    mkid(u,v)

In many applications, it is useful to create a set of identifiers for naming objects in a consistent manner. In most cases, it is sufficient to create such names from two components. The operator `mkid` is provided for this purpose. Its syntax is:
```Julia
R"mkid(U:id,V:id|non-negative integer)"
```
for example
```Julia
julia> Algebra.mkid(:a,3)
:a3

julia> Algebra.mkid(:apple,:s)
:apples
```
while `mkid(:(a+b),2)` gives an error.
""" Reduce.Algebra.mkid

@doc """
    infix(r...)

Users can add new infix operators by using the declarations `infix` and `precedence`. For example,
```Julia
julia> Algebra.infix(:mm)
```
The declaration `infix(:mm)` would allow one to use the symbol `mm` as an infix operator:
`R"a mm b"` instead of `R"mm(a,b)"`.
""" Reduce.Algebra.infix

@doc """
    precedence(a,b)

Users can add new infix operators by using the declarations `infix` and `precedence`. For example,
```Julia
julia> Algebra.precedence(:mm,:-)
```
The declaration `precedence(:mm,:-)` says that `mm` should be inserted into the infix operator precedence list just after the `-` operator. This gives it higher precedence than `-` and lower precedence than `*` . Thus `R"a - b mm c - d"` means `R"a - (b mm c) - d"`, while `R"a * b mm c * d"` means `R"(a * b) mm (c * d)"`.
""" Reduce.Algebra.precedence

@doc """
    order(r...)

The declaration `order` may be used to order variables on output. The syntax is:
```Julia
julia> Algebra.order(v1,...vn)
```
where the `vi` are kernels. Thus,
```Julia
julia> Algebra.order(:x,:y,:z)
```
orders `x` ahead of `y`, `y` ahead of `z` and all three ahead of other variables not given an order. `order(nothing)` resets the output order to the system default. The order of variables may be changed by further calls of `order`, but then the reordered variables would have an order lower than those in earlier `order` calls. Thus,
```Julia
julia> Algebra.order(:x,:y,:z)  

julia> Algebra.order(:y,:x)
```
would order `z` ahead of `y` and `x`. The default ordering is usually alphabetic.
""" Reduce.Algebra.order

@doc """
    factor(r...)


This declaration takes a list of identifiers or kernels as argument. `factor` is not a factoring command (use `factorize` or the `factor` switch for this purpose); rather it is a separation command. All terms involving fixed powers of the declared expressions are printed as a product of the fixed powers and a sum of the rest of the terms.

For example, after the declaration
```Julia
julia> Algebra.factor(:x)
```
the polynomial \$(x + y + 1)^2\$ will be printed as
```
         2                  2  
        x  + 2*x*(y + 1) + y  + 2*y + 1
```
All expressions involving a given prefix operator may also be factored by putting the operator name in the list of factored identifiers. For example:
```Julia
julia> Algebra.factor(:x,:cos,:(sin(x))
```
causes all powers of `x` and `sin(x)` and all functions of `cos` to be factored.
""" Reduce.Algebra.factor

@doc """
    remfac(r...)

The declaration `remfac(v1,...,vn)` removes the factoring flag from the expressions `v1` through `vn`.
""" Reduce.Algebra.remfac

@doc """
    korder(r...)

The internal ordering of variables (more specifically kernels) can have a significant effect on the space and time associated with a calculation. In its default state, REDUCE uses a specific order for this which may vary between sessions. However, it is possible for the user to change this internal order by means of the declaration `korder`. The syntax for this is:
```Julia
julia> Algebra.korder(v1,...,vn)
```
where the `vi` are kernels. With this declaration, the `vi` are ordered internally ahead of any other kernels in the system. `v1` has the highest order, `v2` the next highest, and so on. A further call of `korder` replaces a previous one. `korder(nothing)` resets the internal order to the system default.
""" Reduce.Algebra.korder

@doc """
    realvalued(r...)

The declaration `realvalued` may be used to restrict variables to the real numbers. The syntax is:
```Julia
	Algebra.realvalued(v1,...vn)
```
For such variables the operator `impart` gives the result zero. Thus, with
```Julia
julia> Algebra.realvalued(:x,:y)
```
the expression `impart(x+sin(y))` is evaluated as zero. You may also declare an operator as real valued with the meaning, that this operator maps real arguments always to real values. Example:
```Julia
julia> Algebra.operator(:h); Algebra.realvalued(:h,:x)

julia> Algebra.impart(:(h(x)))
0  
 
julia> Algebra.impart(:(h(w)))
:(impart(h(w)))
```
Such declarations are not needed for the standard elementary functions.
""" Reduce.Algebra.realvalued

@doc """
    notrealvalued(r...)

To remove the `realvalued` propery from a variable or an operator use the declaration `notrealvalued` with the syntax:
```Julia
julia> Algebra.notrealvalued(v1,...vn)
```
""" Reduce.Algebra.notrealvalued

@doc """
    factorize(r...)

It is also possible to factorize a given expression explicitly. The operator `factorize` that invokes this facility is used with the syntax
```Julia
R"factorize(EXPRN:polynomial[,INTEXP:prime integer])"
```
the optional argument of which will be described later. Thus to find and display all factors of the cyclotomic polynomial ``x^{105} - 1``, one could write:
```
julia> Algebra.factorize(:(x^105-1))
```
The result is a list of factor,exponent pairs. In the above example, there is no overall numerical factor in the result, so the results will consist only of polynomials in x. The number of such polynomials can be found by using the operator `length`. If there is a numerical factor, as in factorizing ``12x^2 - 12``, that factor will appear as the first member of the result. It will however not be factored further. Prime factors of such numbers can be found, using a probabilistic algorithm, by turning on the switch `ifactor`. For example,
```
julia> Algebra.on(:ifactor); Algebra.factorize(:(12x^2-12))
```
would result in the output
```
((2, 2), (3, 1), (:(x ^ 2 + 1), 1), (:(x + 1), 1), (:(x - 1), 1))
```
If the first argument of `factorize` is an integer, it will be decomposed into its prime components, whether or not `ifactor` is on.

Note that the `ifactor` switch only affects the result of `factorize`. It has no effect if the `factor` switch is also on.
""" Reduce.Algebra.factorize

@doc """
    remainder(a,b)

This operator is used with the syntax
```Julia
R"REMAINDER(EXPRN1:polynomial,EXPRN2:polynomial)"
```
It returns the remainder when `EXPRN1` is divided by `EXPRN2`. This is the true remainder based on the internal ordering of the variables, and not the pseudo-remainder. The pseudo-remainder and in general pseudo-division of polynomials can be calculated after loading the `polydiv` package. Please refer to the documentation of this package for details.

*Examples:*
```
        remainder((x+y)*(x+2*y),x+3*y) ->  2*y**2  
        remainder(2*x+y,2)             ->  Y
```
""" Reduce.Algebra.remainder

@doc """
    resultant(a,b,var)

This is used with the syntax
```Julia
R"resultant(EXPRN1:polynomial,EXPRN2:polynomial,VAR:kernel)"
```
It computes the resultant of the two given polynomials with respect to the given variable, the coefficients of the polynomials can be taken from any domain. The result can be identified as the determinant of a Sylvester matrix, but can often also be thought of informally as the result obtained when the given variable is eliminated between the two input polynomials. If the two input polynomials have a non-trivial GCD their resultant vanishes.

The switch `bezout` controls the computation of the resultants. It is off by default. In this case a subresultant algorithm is used. If the switch Bezout is turned on, the resultant is computed via the Bezout Matrix. However, in the latter case, only polynomial coefficients are permitted.
""" Reduce.Algebra.resultant

@doc """
    interpol(val,var,mp)

Syntax:
```Julia
R"interpol(⟨values⟩,⟨variable⟩,metapoints)"
```
where `⟨values⟩` and `⟨points⟩` are lists of equal length and `<variable>` is an algebraic expression (preferably a kernel).

`interpol` generates an interpolation polynomial ``f`` in the given variable of degree `length(⟨values⟩)-1`. The unique polynomial ``f`` is defined by the property that for corresponding elements ``v`` of `⟨values⟩` and ``p`` of `⟨points⟩` the relation ``f(p) = v`` holds.

The Aitken-Neville interpolation algorithm is used which guarantees a stable result even with rounded numbers and an ill-conditioned problem.
""" Reduce.Algebra.interpol

@doc """
    deg(p,var)

This operator is used with the syntax
```Julia
R"deg(EXPRN:polynomial,VAR:kernel)"
```
It returns the leading degree of the polynomial `EXPRN` in the variable `VAR`. If `VAR` does not occur as a variable in `EXPRN`, 0 is returned.

*Examples:*
```
        deg((a+b)*(c+2*d)^2,a) ->  1  
        deg((a+b)*(c+2*d)^2,d) ->  2  
        deg((a+b)*(c+2*d)^2,e) ->  0
```
Note also that if `ratarg` is on,
```
        deg((a+b)^3/a,a)       ->  3
```
since in this case, the denominator `a` is considered part of the coefficients of the numerator in `a`. With `ratarg` off, however, an error would result in this case.
""" Reduce.Algebra.deg

@doc """
    lcof(expr,var)

`lcof` is used with the syntax
```Julia
R"lcof(EXPRN:polynomial,VAR:kernel)"
```
It returns the leading coefficient of the polynomial `EXPRN` in the variable `VAR`. If `VAR` does not occur as a variable in `EXPRN`, `EXPRN` is returned.

*Examples:*
```
        lcof((a+b)*(c+2*d)^2,a) ->  c**2+4*c*d+4*d**2  
        lcof((a+b)*(c+2*d)^2,d) ->  4*(a+b)  
        lcof((a+b)*(c+2*d),e)   ->  a*c+2*a*d+b*c+2*b*d
```
""" Reduce.Algebra.lcof

@doc """
    lpower(exprn,var)

Syntax:
```Julia
R"lpower(EXPRN:polynomial,VAR:kernel)"
```
`lpower` returns the leading power of `EXPRN` with respect to `VAR`. If `EXPRN` does not depend on `VAR`, 1 is returned.
*Examples:*
```
        lpower((a+b)*(c+2*d)^2,a) ->  a  
        lpower((a+b)*(c+2*d)^2,d) ->  d**2  
        lpower((a+b)*(c+2*d),e)   ->  1
```
""" Reduce.Algebra.lpower

@doc """
    lterm(exprn,var)

Syntax:
```Julia
R"lterm(EXPRN:polynomial,VAR:kernel)"
```
`lterm` returns the leading term of `EXPRN` with respect to `VAR`. If `EXPRN` does not depend on `VAR`, `EXPRN` is returned.

*Examples:*
```
        lterm((a+b)*(c+2*d)^2,a) ->  a*(c**2+4*c*d+4*d**2)  
        lterm((a+b)*(c+2*d)^2,d) ->  4*d**2*(a+b)  
        lterm((a+b)*(c+2*d),e)   ->  a*c+2*a*d+b*c+2*b*d
```
""" Reduce.Algebra.lterm

@doc """
    reduct(exprn,var)

Syntax:
```Julia
R"reduct(EXPRN:polynomial,VAR:kernel)"
```
Returns the reductum of `EXPRN` with respect to `VAR` (i.e., the part of `EXPRN` left after the leading term is removed). If `EXPRN` does not depend on the variable `VAR`, 0 is returned.

*Examples:*
```
     reduct((a+b)*(c+2*d),a) ->  b*(c + 2*d)  
     reduct((a+b)*(c+2*d),d) ->  c*(a + b)  
     reduct((a+b)*(c+2*d),e) ->  0
```
""" Reduce.Algebra.reduct

@doc """
    totaldeg(expr,var)

Syntax:
```
     totaldeg(a*x^2+b*x+c, x)  => 2  
     totaldeg(a*x^2+b*x+c, {a,b,c})  => 1  
     totaldeg(a*x^2+b*x+c, {x, a})  => 3  
     totaldeg(a*x^2+b*x+c, {x,b})  => 2  
     totaldeg(a*x^2+b*x+c, {p,q,r})  => 0
```
`totaldeg(u, kernlist)` finds the total degree of the polynomial `u` in the variables in `kernlist`. If `kernlist` is not a list it is treated as a simple single variable. The denominator of `u` is ignored, and "degree" here does not pay attention to fractional powers. Mentions of a kernel within the argument to any operator or function (eg `sin`, `cos`, `log`, `sqrt`) are ignored. Really `u` is expected to be just a polynomial.
""" Reduce.Algebra.totaldeg

@doc """
    clear(r...)

The user may remove all assignments and substitution rules from any expression by the command `clear`, in the form
```
R"clear ⟨expression⟩,…,⟨expression⟩ = ⟨terminator⟩"
e.g.
```Julia
julia> Algebra.clear(:x,:(h(x,y)))
```
Because of their *instant evaluation* property, array and matrix elements cannot be cleared with `clear`. For example, if `a` is an array, you must say
```Julia
R"a(3) := 0"
```
rather than
```Julia
R"clear a(3)"
```
to “clear” element `a(3)`.

On the other hand, a whole array (or matrix) `a` can be cleared by the command `clear(:a)`. This means much more than resetting to 0 all the elements of `a`. The fact that `a` is an array, and what its dimensions are, are forgotten, so `a` can be redefined as another type of object, for example an operator.

If you need to clear a variable whose name must be computed, see the `unset` statement.
""" Reduce.Algebra.clear
