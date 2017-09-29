#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

export RExpr, @R_str, parse, rcall, convert, error, ReduceError, ==, getindex
import Base: parse, convert, error, ==, getindex, *, split

type ReduceError <: Exception
    errstr::Compat.String
end

Base.showerror(io::IO, err::ReduceError) = print(io,"Reduce: "*chomp(err.errstr))

function print_args(io::IO,a::Array{Any,1})
    print(io, "(")
    for (i, arg) in enumerate(a)
        show_expr(io, arg)
        i ≠ endof(a) ? print(io,",") : print(io,")")
    end
end

function show_expr(io::IO, expr::Expr) # recursively unparse Julia expression
    if expr.head == :call
        show_expr(io, expr.args[1])
        print_args(io,expr.args[2:end])
    elseif expr.head == :(=)
        show_expr(io,expr.args[1])
        print(io,":=")
        show_expr(io,expr.args[2])
    elseif expr.head == :for
        print(io,"for ")
        show_expr(io,expr.args[1])
        show_expr(io,expr.args[2])
    elseif expr.head == :block
        print(io,"begin ")
        show_expr(io,expr.args[1])
        for k ∈ 2:length(expr.args[2:end])+1
            print(io,";")
            show_expr(io,expr.args[k])
        end
        print(io," end")
    elseif expr.head == :function
        print(io,"procedure ")
        show_expr(io,expr.args[1])
        print(io,";")
        show_expr(io,expr.args[2])
    elseif expr.head == :return
        print(io,"return ")
        show_expr(io,expr.args[1])
    elseif expr.head == :macrocall
        if expr.args[1] == Symbol("@big_str")
            print(io,expr.args[2])
        else
            throw(ReduceError("Macro $(expr.args[1]) block structure not supported"))
        end
    elseif expr.head == :(:)
        show_expr(io,expr.args[1])
        print(io,":")
        show_expr(io,expr.args[2])
        print(io," ")
    elseif expr.head == :line; nothing
    else
        throw(ReduceError("Nested :$(expr.head) block structure not supported"))
    end
end

const infix_ops = ["+", "-", "*", "/", "**"]
isinfix(args) = args in infix_ops

#show_expr(io::IO, ex) = print(io, ex |> string |> JSymReplace)
function show_expr(io::IO, ex)
    edit = IOBuffer()
    print(edit, ex)
    print(io, edit |> String |> JSymReplace)
end

function unparse(expr::Expr)
    str = Array{Compat.String,1}(0)
    io = IOBuffer()
    if expr.head == :block
        for line ∈ expr.args # block structure
            show_expr(io,line)
            push!(str,String(take!(io)))
        end
        return rtrim(str)
    else
        show_expr(io, expr)
        return push!(str,String(io))
    end
end

"""
A Reduce expression

# Summary:
type RExpr <: Any

# Fields:
str :: Array{Compat.String,1}
"""
type RExpr
    str::Array{Compat.String,1}
    RExpr(r::Array{Compat.String,1}) = new(r)
end

RExpr(r::Array{SubString{String},1}) = RExpr(convert(Array{Compat.String,1},r))
RExpr(str::Compat.String) = RExpr(push!(Array{Compat.String,1}(0),str))

function RExpr(r::Any); y = "$r"
    for key ∈ keys(repjlr)
        y = replace(y,key,repjlr[key])
    end
    return RExpr(y)
end

macro R_str(str)
    RExpr(str)
end

*(x::RExpr,y::Compat.String) = RExpr(push!(deepcopy(x.str),y))
*(x::Compat.String,y::RExpr) = RExpr(unshift!(deepcopy(y.str),x))
*(x::RExpr,y::RExpr) = RExpr(vcat(x.str...,y.str...))

function rtrim(r::Array{Compat.String,1})
    n = Array{Compat.String,1}(0)
    for h ∈ 1:length(r)
        !isempty(r[h]) && push!(n,r[h])
    end
    return n
end

function split(r::RExpr)
    n = Array{Compat.String,1}(0)
    for h ∈ 1:length(r.str)
        p = split(replace(r.str[h],r"\$",";"),';')
        for t ∈ 1:length(p)
            push!(n,p[t])
    end; end
    return RExpr(rtrim(n))
end

const r_to_jl = Dict(
    "i"             =>  "im",
    "euler_gamma"   =>  "eulergamma",
    "infinity"      =>  "Inf"
)

const r_to_jl_utf = Dict(
    "pi"            =>  "π",
    "golden_ratio"  =>  "φ",
    "**"            =>  "^",
    "/"             =>  "//"
)

const jl_to_r = Dict(
    "im"            =>  "i",
    "eu"            =>  "euler_gamma",
    "eulergamma"    =>  "euler_gamma",
    "golden"        =>  "golden_ratio",
    "Inf"           =>  "infinity"
)

const jl_to_r_utf = Dict(
    "π"             =>  "pi",
    "γ"             =>  "euler_gamma",
    "φ"             =>  "golden_ratio",
    "^"             =>  "**",
    "//"            =>  "/"
)

# convert substitution dictionary into SUB parameter string
function _syme(syme::Dict{String,String})
    str = ""
    for key in keys(syme)
        str = str*"($key => $(syme[key])),"
    end
    return str[1:end-1]
end

const symrjl = _syme(r_to_jl)
reprjl = r_to_jl_utf
const symjlr = _syme(jl_to_r)
const repjlr = jl_to_r_utf
_subst{T}(syme::String,expr::T) = convert(T, "!*hold($expr)\$ ws where $syme" |> rcall)

Rational = ( () -> begin
        gs = true
        return (tf=gs)->(gs≠tf && (gs=tf; reprjl["/"]=gs?"//":"/"); return gs)
    end)()

ImParse = ( () -> begin
        gs = true
        return (tf=gs)->(gs≠tf && (gs=tf); return gs)
    end)()

function JSymReplace(str::Compat.String)
    for key ∈ keys(repjlr)
        str = replace(str,key,repjlr[key])
    end
    ImParse() && !isinfix(str) && (str = _subst(symjlr,str))
    return str
end

"""
    RExpr(e:Expr)

Convert Julia expression to Reduce expression

# Examples
```julia-repl
julia> RExpr(:(sin(x*im) + cos(y*ϕ)))

     sqrt(5)*y + y
cos(---------------) + sinh(x)*i
           2
```
"""
RExpr(expr::Expr) = expr |> unparse |> RExpr |> split

function RSymReplace(str::String)
    ImParse() && (str = _subst(symrjl,str))
    for key in keys(reprjl)
        str = replace(str,key,reprjl[key])
    end
    return str
end

include("parser.jl")
parsegen(:parse,:expr) |> eval

@doc """
    parse(r::RExpr)

Parse a Reduce expression into a Julia expression

# Examples
```julia-repl
julia> parse(R\"sin(i*x)\")
:(sinh(x) * im)
```
""" parse

convert(::Type{RExpr}, r::RExpr) = r
convert(::Type{Array{Compat.String,1}}, r::RExpr) = r.str
convert(::Type{Compat.String}, r::RExpr) = join(r.str,"; ")
convert{T}(::Type{T}, r::RExpr) = T <: Number ? eval(parse(r)) : parse(r)

"""
    rcall(r::RExpr)

Evaluate a Reduce expression.

# Examples
```julia-repl
julia> R\"int(sin(x), x)\" |> RExpr |> rcall
 - cos(x)
```
"""
function rcall(r::RExpr;
        on::Union{Array{Symbol,1},Array{String,1}}=Symbol[],
        off::Union{Array{Symbol,1},Array{String,1}}=Symbol[])
    typeof(on) == Array{String,1} ? (ona = convert(Array{Symbol,1},on)) : (ona = on)
    typeof(off) == Array{String,1} ? (offa = convert(Array{Symbol,1},off)) : (offa = off)
    ons = ""
    onr = ""
    offs = ""
    offr = ""
    mode = true
    trim = false
    expo = false
    for o in ona
        if o == :expand
            ons = ons*"on exp\$ "
            onr = onr*"; off exp "
        else
            ons = ons*"on $o\$ "
            onr = onr*"; off $o "
        end
        o == :factor && (expo = true)
        o in offa && throw(ReduceError("Invalid: switch on and off at once"))
        o in [:latex,:nat] && (mode = false)
        o == :nat && (trim = true)
    end
    for o in offa
        !(o == :factor) && (offs = offs*"off $o\$ ")
        !(o in [offlist;[:factor]]) && (offr = offr*"; on $o")
    end
    write(rs,ons*offs*string(r)*onr*offr)
    mode ? (sp = readsp(rs)) : (sp = read(rs))
    expo && rcall(R"off exp")
    mode && for h ∈ 1:length(sp)
        sp[h] = replace(sp[h],r"\n","")
        sp[h] = replace(sp[h],r"\\","")
    end
    trim && (return join(split(sp,"\n")[2:end-1],'\n'))
    for o in offa
        o == :nat && (return join(sp))
        o == :latex && shift!(sp)
    end
    return mode ? sp |> RExpr |> split : sp
end

rcall(r::RExpr,switches...) = rcall(r;on=[switches...])

"""
    rcall{T}(e::T)

Evaluate a Julia expression or string using the Reduce interpretor and convert
output back into the input type

# Examples
```julia-repl
julia> rcall(\"int(sin(y)^2, y)\")
\"( - cos(y)*sin(y) + y)/2\"
julia> rcall(:(int(1/(1+x^2), x)))
:(atan(x))
```
"""
function rcall{T}(expr::T;on::Array{Symbol,1}=Symbol[],off::Array{Symbol,1}=Symbol[])
    comp = rcall(RExpr(expr);on=on,off=off)
    (:latex in on) | (:nat in on) ? (return comp) : (return convert(T,comp))
end

rcall(r,switches...) = rcall(r;on=Symbol[switches...])

function ==(r::RExpr, s::RExpr)
    n = split(r).str
    m = split(s).str
    l=length(n)
    l≠length(m) && (return false)
    b = true
    for j∈1:l
        b &= "if($(n[j]))=($(m[j]))then 1 else 0"|>rcall|>parse|>eval|>Bool
    end
    return b
end

#getindex(r::RExpr, i) = "$r($i)" |> rcall
