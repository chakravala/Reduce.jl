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
    :deg,
    :lcof,
    :lpower,
    :lterm,
    :reduct,
    :totaldeg,
    :pochhammer,
    :fibonaccip,
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

Make variable substitutions using Reduce's native sub command
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

``(a)k	= \frac{\Gamma (a+-k-)}{\Gamma (a)}``

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
The `tag` argument is used to uniquely identify those particular solutions. Solution multiplicities are stored in the global variable `root_multiplicities` rather than the solution list. The value of this variable is a list of the multiplicities of the solutions for the last call of `solve`. For example,
```Julia
julia> Algebra.solve(:(x^2==2x-1),:x); Reduce.root_multiplicities()
```
gives the results
```Julia
(:(x = 1),)
 
(2,)
```
If you want the multiplicities explicitly displayed, the switch `multiplicities` can be turned on. For example
```Julia
julia> Algebra.on(:multiplicities); Algebra.solve(:(x^2==2x-1),:x)
```
yields the result
```Julia
(:(x = 1), :(x = 1))
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
f(y/z,x) 	-> f(y,x)/z		if z does not depend on x
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
