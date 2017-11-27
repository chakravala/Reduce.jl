#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

export RExpr, @RExpr, @R_str, rcall, @rcall, convert, ==, getindex, string, show
import Base: parse, convert, ==, getindex, *, split, string, show

"""
Reduce expression

### Summary:
type RExpr <: Any

### Fields:
str::Array{Compat.String,1}
"""
type RExpr
    str::Array{Compat.String,1}
    RExpr(r::Array{Compat.String,1}) = new(r)
end

RExpr(r::Array{SubString{String},1}) = RExpr(convert(Array{Compat.String,1},r))
RExpr(str::Compat.String) = RExpr(push!(Array{Compat.String,1}(0),str))

"""
    RExpr(e::Expr)

Convert Julia expression to Reduce expression

## Examples
```julia-repl
julia> RExpr(:(sin(x*im) + cos(y*ϕ)))

     sqrt(5)*y + y
cos(---------------) + sinh(x)*i
           2
```
"""
RExpr(expr::Expr) = expr |> unparse |> RExpr |> split

function RExpr(r::Matrix)
    out = IOBuffer()
    show_expr(out,r)
    return out |> String |> RExpr
end

function RExpr(r::Any)
    typeof(r) <: AbstractFloat && isinf(r) && (return RExpr((r > 0 ? "" : "-")*"infinity"))
    y = "$r"
    for key ∈ keys(repjlr)
        y = replace(y,key,repjlr[key])
    end
    return RExpr(y)
end

macro RExpr(r)
    RExpr(r)
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
        p = split(replace(r.str[h],r"(\$)|(;\n)",";"),r"(?<!\!#[0-9a-fA-F]{4});")
        for t ∈ 1:length(p)
            push!(n,p[t])
    end; end
    return RExpr(rtrim(n))
end

string(r::RExpr) = convert(Compat.String,r)
show(io::IO, r::RExpr) = print(io,convert(Compat.String,r))

@compat function show(io::IO, ::MIME"text/plain", r::RExpr)
    length(r.str) > 1 && (print(io,string(r)*";"); return nothing)
    print(io,rcall(r;on=[:nat]) |> string |> chomp)
end

@compat function show(io::IO, ::MIME"text/latex", r::RExpr)
    rcall(R"on latex")
    write(rs,r)
    rd = readsp(rs)
    rcall(R"off latex")
    sp = split(join(rd),"\n\n")
    print(io,"\\begin{eqnarray}\n")
    ct = 0 # add enumeration
    for str ∈ sp
        ct += 1 
        length(sp) ≠ 1 && print(io,"($ct)"*'&'*"\\,")
        print(io,replace(str,r"(\\begin{displaymath})|(\\end{displaymath})",""))
        ct ≠ length(sp) && print(io,"\\\\\\\\")
    end # new line
    print(io,"\n\\end{eqnarray}")
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

_subst{T}(syme::String,expr::T) = convert(T, "!*hold($expr)\$ ws where $syme" |> rcall)

const symrjl = _syme(r_to_jl)
reprjl = r_to_jl_utf
const symjlr = _syme(jl_to_r)
const repjlr = jl_to_r_utf

"""
    Reduce.Rational(::Bool)

Toggle whether to use '/' or '//' for division in julia expressions
"""
Rational = ( () -> begin
        gs = true
        return (tf=gs)->(gs≠tf && (gs=tf; reprjl["/"]=gs?"//":"/"); return gs)
    end)()

"""
    Reduce.SubCall(::Bool)

Toggle whether to substitute additional expressions
"""
SubCall = ( () -> begin
        gs = true
        return (tf=gs)->(gs≠tf && (gs=tf); return gs)
    end)()

function JSymReplace(str::Compat.String)
    for key ∈ keys(repjlr)
        str = replace(str,key,repjlr[key])
    end
    SubCall() && !isinfix(str) && (str = _subst(symjlr,str))
    contains(str,"!#") && (str = replace(rcall(str,:nat),r"\n",""))
    return str
end

function RSymReplace(str::String)
    clean = replace(str,r"[ ;\n]","")
    paren = ismatch(r"^\(((?>[^\(\)]+)|(?R))*\)$",clean)
    (isempty(clean)|(clean=="()")) && (return str)
    SubCall() && !isinfix(str) && (str = _subst(symrjl,str))
    if contains(str,"!#")
        rsp = split(str,';')
        for h in 1:length(rsp)
            if contains(rsp[h],"!#")
                sp = split(rsp[h],r"!#")
                rsp[h] = join([sp[1],replace(rcall("!#"*sp[end]*";",:nat),r"\n","")])
            end
        end
        str = join(rsp)
    end
    for key in keys(reprjl)
        str = replace(str,key,reprjl[key])
    end
    str == "inf" && (str = "Inf")
    str == " - inf" && (str = "-Inf")
    return paren ? "("*str*")" : str
end

RSymReplace(str::SubString{String}) = str |> String |> RSymReplace

convert(::Type{RExpr}, r::RExpr) = r
convert(::Type{Array{Compat.String,1}}, r::RExpr) = r.str
convert(::Type{Compat.String}, r::RExpr) = join(r.str,";\n")
convert{T}(::Type{T}, r::RExpr) = T <: Number ? eval(parse(r)) : parse(r)

"""
    rcall(r::RExpr)

Evaluate a Reduce expression.

## Examples
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
    rlfi = false
    for o in ona
        if o == :expand
            ons = ons*"on exp\$ "
            onr = onr*"; off exp "
        elseif o == :latex
            rcall(R"on latex")
            rlfi = true
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
        sp[h] = replace(sp[h],'\n',"")
        sp[h] = replace(sp[h],'\\',"")
    end
    trim && (return join(split(sp,"\n")[2:end-1],'\n'))
    rlfi && rcall(R"off latex")
    for o in offa
        o == :nat && (return join(sp))
        o == :latex && shift!(sp)
    end
    return mode ? sp |> RExpr |> split : sp
end

rcall(r::RExpr,switches...) = rcall(r;on=[switches...])

macro rcall(r)
    :(eval(rcall($r)))
end

"""
    rcall{T}(e::T)

Evaluate a Julia expression or string using the Reduce interpretor and convert
output back into the input type

## Examples
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
