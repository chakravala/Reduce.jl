#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

export RExpr, @RExpr, @R_str, rcall, @rcall, convert, ==, string, show, sub, squash
import Base: parse, convert, ==, getindex, *, split, string, show

"""
Reduce expression

### Summary:
type RExpr <: Any

### Fields:
str::Array{Compat.String,1}
"""
struct RExpr
    str::Array{Compat.String,1}
    RExpr(r::Array{Compat.String,1}) = new(r)
end

RExpr(r::Array{SubString{String},1}) = RExpr(convert(Array{Compat.String,1},r))
RExpr(str::Compat.String) = RExpr([str])

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
    return out |> take! |> String |> RExpr
end

function RExpr(r::Any)
    typeof(r) <: AbstractFloat && isinf(r) && (return RExpr((r > 0 ? "" : "-")*"infinity"))
    y = "$r"
    for key ∈ keys(repjlr)
        y = replace(y, key => repjlr[key])
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
    n = deepcopy(r)
    h = 1
    l = length(r)
    while h ≤ l
        isempty(r[h]) ? (deleteat!(n,h); l -= 1) : (h += 1)
    end
    return n
end

function split(r::RExpr)
    n = Compat.String[]
    for h ∈ 1:length(r.str)
        p = split(replace(r.str[h],r"(\$)|(;\n)"=>";"),r"(?<!\!#[0-9a-fA-F]{4});")
        for t ∈ 1:length(p)
            push!(n,p[t])
    end; end
    return RExpr(rtrim(n))
end

string(r::RExpr) = convert(Compat.String,r)
show(io::IO, r::RExpr) = print(io,convert(Compat.String,r))

@compat function show(io::IO, ::MIME"text/plain", r::RExpr)
    length(r.str) > 1 && (print(io,string(r),";"); return nothing)
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
        print(io,replace(str,r"(\\begin{displaymath})|(\\end{displaymath})"=>""))
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
    str = IOBuffer()
    for key in keys(syme)
        write(str,"($key => $(syme[key])),")
    end
    return String(take!(str)[1:end-1])
end

function _subst(syme::String,expr::T) where T
    convert(T, "!*hold($expr)\$ ws where $syme" |> rcall)
end

const symrjl = _syme(r_to_jl)
const symjlr = _syme(jl_to_r)
reprjl = r_to_jl_utf
const repjlr = jl_to_r_utf
const gexrjl = Regex("($(join(keys(r_to_jl),")|(")))")
const gexjlr = Regex("($(join(keys(jl_to_r),")|(")))")

sub(syme::String,expr::RExpr) = "sub($syme,$expr)" |> rcall |> RExpr
sub(syme::String,expr::T) where T = convert(T,sub(syme,RExpr(expr)))
sub(s::Dict{String,String},expr) = sub(_syme(s),expr)
sub(s::Dict{<:Any,<:Any},expr) = sub(Dict([=>(string.(RExpr.([b[1],b[2]]))...) for b ∈ collect(s)]...),expr)
sub(s::Pair{<:Any,<:Any},expr) = sub(Dict(s),expr)
sub(s::Array{Pair{<:Any,<:Any},1},expr) = sub(Dict(s...),expr)

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

"""
    Reduce.Rational(::Bool)

Toggle whether to use '/' or '//' for division in julia expressions
"""
Rational = ( () -> begin
        gs = true
        return (tf=gs)->(gs≠tf && (gs=tf; reprjl["/"]=gs ? "//" : "/"); return gs)
    end)()

"""
    Reduce.SubCall(::Bool)

Toggle whether to substitute additional expressions
"""
SubCall = ( () -> begin
        gs = true
        return (tf=gs)->(gs≠tf && (gs=tf); return gs)
    end)()

"""
    Reduce.SubHold(::Real)

Sleep timer in case of clogged Reduce pipe on SubCall
"""
SubHold = ( () -> begin
        gs = 1/17
        return (tf=gs)->(gs≠tf && (gs=tf); return gs)
    end)()

"""
    Reduce.SubFail(::Integer)

Failure limit in case of clogged Reduce pipe on SubCall
"""
SubFail = ( () -> begin
        gs = 17
        return (tf=gs)->(gs≠tf && (gs=tf); return gs)
    end)()
 
@inline function SubReplace(sym::Symbol,str::String)
    a = matchall(r"([^ ()+*\^\/-]+|[()+*\^\/-])",str)
    for s ∈ 1:length(a)
        if !isinfix(a[s]) && !contains(a[s],r"[()]") && 
            contains(a[s], sym == :r ? gexrjl : gexjlr)
            w = _subst(sym == :r ? symrjl : symjlr, a[s])
            if w == ""
                c = 1
                f = SubFail()
                h = SubHold()
                while w == "" && c < f
                    sleep(sqrt(c)*h)
                    w = _subst(sym == :r ? symrjl : symjlr, a[s])
                    c += 1
                end
                warn("Reduce pipe clogged, $(w ≠ "" ? "success" : "failure") after $c tries")
            end
            w ≠ "" && (a[s] = w)
        end
    end
    return join(a)
end

@noinline function JSymReplace(str::Compat.String)
    for key ∈ keys(repjlr)
        str = replace(str, key => repjlr[key])
    end
    SubCall() && !isinfix(str) && (str = SubReplace(:jl,str))
    contains(str,"!#") && (str = replace(rcall(str,:nat),r"\n"=>""))
    return str
end

@noinline function RSymReplace(str::String)
    clean = replace(str,r"[ ;\n]"=>"")
    paren = contains(clean,r"^\(((?>[^\(\)]+)|(?R))*\)$")
    (isempty(clean)|(clean=="()")) && (return str)
    SubCall() && !isinfix(str) && (str = SubReplace(:r,str))
    if contains(str,"!#")
        rsp = split(str,';')
        for h in 1:length(rsp)
            if contains(rsp[h],"!#")
                sp = split(rsp[h],r"!#")
                rsp[h] = join([sp[1],replace(rcall("!#"*sp[end]*";",:nat),r"\n"=>"")])
            end
        end
        str = join(rsp)
    end
    for key in keys(reprjl)
        str = replace(str, key => reprjl[key])
    end
    str == "inf" && (str = "Inf")
    str == " - inf" && (str = "-Inf")
    return paren ? "("*str*")" : str
end

RSymReplace(str::SubString{String}) = str |> String |> RSymReplace

convert(::Type{RExpr}, r::RExpr) = r
convert(::Type{Array{Compat.String,1}}, r::RExpr) = r.str
convert(::Type{Compat.String}, r::RExpr) = join(r.str,";\n")
convert(::Type{T}, r::RExpr) where T = T <: Number ? eval(parse(r)) : parse(r)

"""
    rcall(r::RExpr)

Evaluate a Reduce expression.

## Examples
```julia-repl
julia> R\"int(sin(x), x)\" |> RExpr |> rcall
 - cos(x)
```
"""
@noinline function rcall(r::RExpr;
        on::Union{Array{Symbol,1},Array{String,1}}=Symbol[],
        off::Union{Array{Symbol,1},Array{String,1}}=Symbol[])
    typeof(on) == Array{String,1} ? (ona = convert(Array{Symbol,1},on)) : (ona = on)
    typeof(off) == Array{String,1} ? (offa = convert(Array{Symbol,1},off)) : (offa = off)
    ons = IOBuffer()
    onr = IOBuffer()
    offs = IOBuffer()
    offr = IOBuffer()
    mode = true
    trim = false
    expo = false
    rlfi = false
    for o in ona
        if o == :expand
            write(ons,"on exp\$ ")
            write(onr,"; off exp ")
        elseif o == :latex
            rcall(R"on latex")
            rlfi = true
        else
            write(ons,"on $o\$ ")
            write(onr,"; off $o ")
        end
        o == :factor && (expo = true)
        o in offa && throw(ReduceError("Invalid: switch on and off at once"))
        o in [:latex,:nat] && (mode = false)
        o == :nat && (trim = true)
    end
    for o in offa
        !(o == :factor) && write(offs,"off $o\$ ")
        !(o in [offlist;[:factor]]) && write(offr,"; on $o")
    end
    write(rs,String(UInt8[take!(ons)...,take!(offs)...]) *
          string(r) *
          String(UInt8[take!(onr)...,take!(offr)...]))
    mode ? (sp = readsp(rs)) : (sp = read(rs))
    expo && rcall(R"off exp")
    mode && for h ∈ 1:length(sp)
        sp[h] = replace(sp[h],'\n' => "")
        sp[h] = replace(sp[h],'\\' => "")
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
function rcall(expr::T;on::Array{Symbol,1}=Symbol[],off::Array{Symbol,1}=Symbol[]) where T
    comp = rcall(RExpr(expr);on=on,off=off)
    (:latex in on) | (:nat in on) ? (return comp) : (return convert(T,comp))
end

rcall(r,switches...) = rcall(r;on=Symbol[switches...])

function ==(r::RExpr, s::RExpr)
    n = expand(r).str
    m = expand(s).str
    l=length(n)
    l≠length(m) && (return false)
    b = true
    for j∈1:l
        b &= "if($(n[j]))=($(m[j]))then 1 else 0"|>rcall|>parse|>eval|>Bool
    end
    return b
end

#getindex(r::RExpr, i) = "$r($i)" |> rcall

function callcount(expr)
    c = 0
    if typeof(expr) == Expr
        if expr.head == :call
            c += 1
        end
        for arg ∈ expr.args
            c += callcount(arg)
        end
    end
    return c
end

#= regroup parens * add feature

@noinline function squash(expr)
    if typeof(expr) == Expr && expr.head ∈ [:block,:function]
        nex = deepcopy(expr)
        k = expr.head ≠ :function ? 1 : 2
        while k ≤ length(nex.args)
            found = false
            if typeof(nex.args[k]) == Expr &&
                    contains(string(nex.args[k].head),r"[*\/+-^]=$")
                var = nex.args[k].args[1]
                for h ∈ 1:k-1
                    if typeof(nex.args[h]) == Expr &&
                            nex.args[h].head == :(=) &&
                            nex.args[h].args[1] == var
                        nex.args[h].args[2] = eval(Expr(:call,
                            Symbol(match(r"[^(=$)]",string(nex.args[k].head)).match),
                            QuoteNode(nex.args[h].args[2]),
                            QuoteNode(nex.args[k].args[2])))
                        deleteat!(nex.args,k)
                        found = true
                        break
                    end
                end
            else
                typeof(nex.args[k]) == Expr && (nex.args[k] = squash(nex.args[k]))
            end
            !found && (k += 1)
        end
        return length(nex.args) > 1 ? nex : nex.args[end]
    elseif typeof(expr)
    else
        return expr
    end
end=#

function squash(expr)
    typeof(expr) == Expr && if expr.head == :block
        return @eval $expr
    elseif expr.head == :function
        out = deepcopy(expr)
        out.args[2] = @eval $(Expr(:block,expr.args[2]))
        return out
    else
        return rcall(expr)
    end
end
