#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

export RExpr, @R_str, parse, rcall, convert, error, ReduceError, ==, getindex
import Base: parse, convert, error, ==, getindex, *, split

type ReduceError <: Exception
    errstr::Compat.String
end

Base.showerror(io::IO, err::ReduceError) = print(io,"Reduce:"*chomp(err.errstr))

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
            error("Macro $(expr.args[1]) block structure not implemented")
        end
    elseif expr.head == :(:)
        show_expr(io,expr.args[1])
        print(io,":")
        show_expr(io,expr.args[2])
        print(io," ")
    elseif expr.head == :line; nothing
    else
        error("Nested :$(expr.head) block structure not supported by Reduce.jl")
    end
end

show_expr(io::IO, ex) = print(io, ex |> string |> JSymReplace)

function unparse(expr::Expr)
    str = Array{Compat.String,1}(0)
    io = IOBuffer()
    if expr.head == :block
        for line ∈ expr.args # block structure
            show_expr(io,line)
            push!(str,String(take!(io)))
        end
        str[1] = "begin "*str[1]; str[end] = str[end]*" end"
        return rtrim(str)
    else
        show_expr(io, expr)
        return push!(str,String(io))
    end
end

"""
A Reduce expression
## Summary:
type RExpr <: Any
## Fields:
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
    "euler_gamma"   =>  "eulergamma",
    "infinity"      =>  "Inf"
)

const r_to_jl_utf = Dict(
    "pi"            =>  "π",
    "golden_ratio"  =>  "φ",
    "**"            =>  "^",
    #":="            =>  "=",
    "/"             =>  "//"
)

const jl_to_r = Dict(
    #"eu"            =>  "euler_gamma",
    "eulergamma"    =>  "euler_gamma",
    "golden"        =>  "golden_ratio",
    "Inf"           =>  "infinity"
)

const jl_to_r_utf = Dict(
    "π"             =>  "pi",
    "γ"             =>  "euler_gamma",
    "φ"             =>  "golden_ratio",
    "^"             =>  "**",
    #"="             =>  ":=",
    "//"            =>  "/"
)

# convert substitution dictionary into SUB parameter string
function _syme(syme::Dict{String,String})
    str = ""
    for key in keys(syme)
        str = str*"($key)=($(syme[key])),"
    end
    return str[1:end-1]
end

const symrjl = _syme(r_to_jl)
reprjl = Dict(r_to_jl...,r_to_jl_utf...)
const symjlr = _syme(jl_to_r)
const repjlr = Dict(jl_to_r...,jl_to_r_utf...)
# _subst(syme::String,expr) = "sub({$syme},$expr)" |> RExpr |> rcall

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
    ImParse() && (str = str*"\$ ws where im => i" |> rcall)
    return str
end

"""
  RExpr(expr::Expr)
Convert Julia expression to Reduce expression
## Examples
```julia
julia> RExpr(:(sin(x*im) + cos(y*ϕ)))

     sqrt(5)*y + y
cos(---------------) + sinh(x)*i
           2
```
"""
RExpr(expr::Expr) = expr |> unparse |> RExpr

function RSymReplace(str::Compat.String)
    ImParse() && (str = str*"\$ ws where i => im" |> rcall)
    for key in keys(reprjl)
        str = replace(str,key,reprjl[key])
    end
    return str
end

"""
  parse(rexpr::RExpr)
Parse a Reduce expression into a Julia expression
## Examples
```julia
julia> parse(R\"sin(i*x)\")
:(sinh(x) * im)
```
"""
function parse(r::RExpr,be=0)
    pexpr = Array{Any,1}(0)
    sexpr = split(r).str
    iter = 1:length(sexpr)
    state = start(iter); #show(sexpr)
    while !done(iter,state)
        (h,state) = next(iter, state)
        sh = split(sexpr[h],r"[ ]+")
        en = 1
        isempty(replace(sh[en]," ","")) && (en = 2); #show(sh[en])
        if contains(sh[en],"procedure")
            js = join(split(sexpr[h],"procedure")[2:end],"procedure")
            (h,state) = next(iter, state)
            y = h
            (h,state) = bematch(sexpr[h],sexpr,h,iter,state)
            push!(pexpr,Expr(:function,parse(js),rparse(sexpr[y:h],be)))
        elseif contains(sh[en],"begin")
            js = join(split(sexpr[h],"begin")[2:end],"begin")
            ep = Array{Any,1}(length(0))
            sh1 = split(js,r"[ ]+")
            c = sum(sh1.=="begin")-sum(sh1.=="end")
            flag = c ≥ 0
            c ≤ -1 && (js = join(split(js,"end")[1:end+c],"end"))
            y = h
            (h,state) = bematch(js,sexpr,h,iter,state)
            ep[1] = rparse(vcat(js,sexpr[y+1:h]...),be+1)
            ep[1] == nothing && shift!(ep)
            while !done(iter,state) & flag
                (h,state) = next(iter, state)
                cQ = c
                js = sexpr[h]
                sh2 = split(js,r"[ ]+")
                c += sum(sh2.=="begin")-sum(sh2.=="end")
                c ≤ -1 && (js = join(split(js,"end")[1:end+c],"end"))
                y = h
                (h,state) = bematch(js,sexpr,h,iter,state)
                epr = rparse(vcat(js,sexpr[y+1:h]...),cQ)
                epr ≠ nothing && push!(ep,epr)
            end
            push!(pexpr,Expr(:block,ep...))
        elseif contains(sh[en],"return")
            js = join(split(sexpr[h],"return")[2:end],"return")
            y = h
            (h,state) = bematch(js,sexpr,h,iter,state)
            rp = rparse(vcat(js,sexpr[y+1:h]...),be)
            push!(pexpr,Expr(:return,rp))
        elseif contains(sh[en],"end")
            nothing
        elseif isempty(sh[en])
            nothing
        elseif contains(sexpr[h],":=")
            sp = split(sexpr[h],":=")
            push!(pexpr,Expr(:(=),parse(sp[1]),rparse(sp[2],be)))
        elseif contains(sexpr[h],":")
            sp = split(sexpr[h],":")
            push!(pexpr,Expr(:(:),rparse(sp[1],be),rparse(sp[2],be)))
        else
            js=sexpr[h]
            se=sum(sh.=="end")
            0<se≤be ? (js=replace(js,"end","")) :
                (se>be && (js=join(split(js,"end")[1:end-be],"end")))
            push!(pexpr,parse(RSymReplace(js)))
        end
    end
    u = length(pexpr)
    return u==1 ? pexpr[1] : (u==0 ? nothing : Expr(:block,pexpr...))
end

rparse(r,be=0) = parse(r |> RExpr, be)
rparse(r::Array{Compat.String,1},be=0) = parse(RExpr(r),be)

function bematch(js,sexpr,h,iter,state)
    sh = split(js,r"[ ]+")
    y = h
    c = sum(sh.=="begin")-sum(sh.=="end")
    flag = c > 0
    while !done(iter,state) & flag
        (y,state) = next(iter, state)
        sh2 = split(sexpr[y],r"[ ]+")
        c += sum(sh2.=="begin")-sum(sh2.=="end")
        flag = c > 0
    end
    return (y,state)
end

convert(::Type{RExpr}, r::RExpr) = r
convert(::Type{Array{Compat.String,1}}, r::RExpr) = r.str
convert(::Type{Compat.String}, r::RExpr) = join(r.str,"; ")
convert{T}(::Type{T}, r::RExpr) = T <: Number ? eval(parse(r)) : parse(r)

"""
  rcall(r::RExpr)
Evaluate a Reduce expression.
## Examples
```julia
julia> R\"int(sin(x), x)\" |> RExpr |> rcall
 - cos(x)
```
"""
function rcall(r::RExpr)
    write(rs,r)
    sp = readsp(rs)
    for h ∈ 1:length(sp)
        sp[h] = replace(sp[h],r"\n","")
        sp[h] = replace(sp[h],r"\\","")
    end
    return RExpr(sp)
end

"""
  rcall{T}(expr::T)
Evaluate a Julia expression or string using the Reduce interpretor and convert
output back into the input type
## Examples
```julia
julia> rcall(\"int(sin(y)^2, y)\")
\"( - cos(y)*sin(y) + y)/2\"
julia> rcall(:(int(1/(1+x^2), x)))
:(atan(x))
```
"""
rcall{T}(expr::T) = convert(T, rcall(RExpr(expr)))

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

getindex(r::RExpr, i) = "$r($i)" |> rcall
