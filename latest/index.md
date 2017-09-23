
<a id='Reduce.jl-Documentation-1'></a>

# Reduce.jl Documentation

- [Reduce.jl Documentation](index.md#Reduce.jl-Documentation-1)
    - [Functions](index.md#Functions-1)
    - [Index](index.md#Index-1)


<a id='Functions-1'></a>

## Functions

<a id='Reduce.Reset' href='#Reduce.Reset'>#</a>
**`Reduce.Reset`** &mdash; *Function*.



Reduce.Reset() Kills the REDUCE process and starts a new instance.

**Examples**

```julia
julia> Reduce.Reset()
Reduce (Free PSL version, revision 4015),  5-May-2017 ...
```


<a target='_blank' href='github.com/chakravala/Reduce.jl.git' class='documenter-source'>source</a><br>

<a id='Reduce.RExpr' href='#Reduce.RExpr'>#</a>
**`Reduce.RExpr`** &mdash; *Type*.



A Reduce expression

**Summary:**

type RExpr <: Any

**Fields:**

str :: Array{Compat.String,1}


<a target='_blank' href='github.com/chakravala/Reduce.jl.git' class='documenter-source'>source</a><br>

<a id='Base.parse' href='#Base.parse'>#</a>
**`Base.parse`** &mdash; *Function*.



parse(rexpr::RExpr) Parse a Reduce expression into a Julia expression

**Examples**

```julia
julia> parse(R"sin(i*x)")
:(sinh(x) * im)
```


<a target='_blank' href='github.com/chakravala/Reduce.jl.git' class='documenter-source'>source</a><br>

<a id='Reduce.rcall' href='#Reduce.rcall'>#</a>
**`Reduce.rcall`** &mdash; *Function*.



rcall(r::RExpr) Evaluate a Reduce expression.

**Examples**

```julia
julia> R"int(sin(x), x)" |> RExpr |> rcall
 - cos(x)
```


<a target='_blank' href='github.com/chakravala/Reduce.jl.git' class='documenter-source'>source</a><br>


rcall{T}(expr::T) Evaluate a Julia expression or string using the Reduce interpretor and convert output back into the input type

**Examples**

```julia
julia> rcall("int(sin(y)^2, y)")
"( - cos(y)*sin(y) + y)/2"
julia> rcall(:(int(1/(1+x^2), x)))
:(atan(x))
```


<a target='_blank' href='github.com/chakravala/Reduce.jl.git' class='documenter-source'>source</a><br>


<a id='Index-1'></a>

## Index

- [`Reduce.RExpr`](index.md#Reduce.RExpr)
- [`Base.parse`](index.md#Base.parse)
- [`Reduce.Reset`](index.md#Reduce.Reset)
- [`Reduce.rcall`](index.md#Reduce.rcall)

