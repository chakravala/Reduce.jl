#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

export RExpr, @R_str, parse, rcall, convert, error, ReduceError, ==, getindex
import Base: parse, convert, error, ==, getindex, *, split

type ReduceError <: Exception; errstr::Compat.String; end
Base.showerror(io::IO, err::ReduceError) = print(io,"Reduce:"*chomp(err.errstr))

function print_args(io::IO,a::Array{Any,1}); print(io, "("); for (i, arg) in enumerate(a)
    show_expr(io, arg); i≠endof(a) ? print(io,","):print(io,")"); end; end
function show_expr(io::IO, expr::Expr) # recursively unparse Julia expression
  if expr.head == :call; show_expr(io, expr.args[1]); print_args(io,expr.args[2:end])
  elseif expr.head == :(=); print(io,"="); print_args(io,expr.args)
  elseif expr.head == :macrocall
    if expr.args[1] == Symbol("@big_str"); print(io,expr.args[2])
    else; error("Macro $(expr.args[1]) block structure not implemented") end
  else; error("Nested :$(expr.head) block structure not supported by Reduce.jl"); end; end
show_expr(io::IO, ex) = print(io, ex)
function unparse(expr::Expr); str = Array{Compat.String,1}(0); io = IOBuffer()
  if expr.head == :block; for line ∈ expr.args # block structure
      show_expr(io,line); push!(str,String(take!(io))); end; return str
  else; show_expr(io, expr); return push!(str,String(io)); end; end

"""
A Reduce expression
## Summary:
type RExpr <: Any
## Fields:
str :: Array{Compat.String,1}
"""
type RExpr; str::Array{Compat.String,1}; RExpr(r::Array{Compat.String,1}) = new(r); end
RExpr(r::Array{SubString{String},1}) = RExpr(convert(Array{Compat.String,1},r))
RExpr(str::Compat.String) = RExpr(push!(Array{Compat.String,1}(0),str))
function RExpr(r::Any); y = "$r"
  for key ∈ keys(repjlr); y = replace(y,key,repjlr[key]); end; return RExpr(y); end
macro R_str(str); RExpr(str); end
*(x::RExpr,y::Compat.String) = RExpr(push!(deepcopy(x.str),y))
*(x::Compat.String,y::RExpr) = RExpr(unshift!(deepcopy(y.str),x))
*(x::RExpr,y::RExpr) = RExpr(vcat(x.str...,y.str...))
function split(r::RExpr); n = Array{Compat.String,1}(0)
  for h ∈ 1:length(r.str); p = split(replace(r.str[h],r"\$",";"),';')
    for t ∈ 1:length(p); push!(n,p[t]); end; end; return RExpr(n); end

const r_to_jl = Dict(
  "i"             =>  "im",
  "euler_gamma"   =>  "eulergamma",
  "infinity"      =>  "Inf")

const r_to_jl_utf = Dict(
  "pi"            =>  "π",
  "golden_ratio"  =>  "φ",
  "**"            =>  "^",
  ":="            =>  "=",
  "/"             =>  "//")

const jl_to_r = Dict(
  "eu"            =>  "e",
  "eulergamma"    =>  "euler_gamma",
  "golden"        =>  "golden_ratio",
  "im"            =>  "i",
  "Inf"           =>  "infinity")

const jl_to_r_utf = Dict(
  "π"             =>  "pi",
  "γ"             =>  "euler_gamma",
  "φ"             =>  "golden_ratio",
  "^"             =>  "**",
  "="             =>  ":=",
  "//"            =>  "/")

# convert substitution dictionary into SUB parameter string
function _syme(syme::Dict{String,String}); str = ""; for key in keys(syme)
  str = str*"($key)=($(syme[key])),"; end; return str[1:end-1]; end

const symrjl = _syme(r_to_jl); const reprjl = Dict(r_to_jl...,r_to_jl_utf...)
const symjlr = _syme(jl_to_r); const repjlr = Dict(jl_to_r...,jl_to_r_utf...)
# _subst(syme::String,expr) = "sub({$syme},$expr)" |> RExpr |> rcall

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
function RExpr(expr::Expr)
  str = unparse(expr)
  for h ∈ 1:length(str)
    for key ∈ keys(repjlr)
      str[h] = replace(str[h],key,repjlr[key]); end; end
  return str |> RExpr; end

"""
  parse(rexpr::RExpr)
Parse a Reduce expression into a Julia expression
## Examples
```julia
julia> parse(R\"sin(i*x)\")
:(sinh(x) * im)
```
"""
function parse(r::RExpr)
  pexpr = Array{Any,1}(0); sexpr = split(r).str
  for h ∈ 1:length(sexpr)
    for key in keys(reprjl)
      sexpr[h] = replace(sexpr[h],key,reprjl[key]); end
    push!(pexpr,parse(sexpr[h])); end
  return length(pexpr) == 1 ? pexpr[1] : Expr(:block,pexpr...); end

convert(::Type{RExpr}, r::RExpr) = r
convert(::Type{Array{Compat.String,1}}, r::RExpr) = r.str
convert(::Type{Compat.String}, r::RExpr) = join(r.str,"; ")
convert{T}(::Type{T}, r::RExpr) = T <: Number ? eval(parse(r)) : parse(r)
if VERSION < v"0.5.0"
  convert(::Type{UTF8String}, r::RExpr) = UTF8String(r.str)
  convert(::Type{ASCIIString}, r::RExpr) = ASCIIString(r.str)
end

"""
  rcall(r::RExpr)
Evaluate a Reduce expression.
## Examples
```julia
julia> R\"int(sin(x), x)\" |> RExpr |> rcall
 - cos(x)
```
"""
function rcall(r::RExpr); write(rs,r); sp = readsp(rs)
  for h ∈ 1:length(sp); sp[h] = replace(sp[h],r"\n","")
    sp[h] = replace(sp[h],r"\\",""); end; return RExpr(sp); end

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

function ==(r::RExpr, s::RExpr); n = split(r).str; m = split(s).str
  l=length(n); l≠length(m) && (return false); b = true; for j∈1:l
    b &= "if($(n[j]))=($(m[j]))then 1 else 0"|>rcall|>parse|>eval|>Bool; end; return b; end
getindex(r::RExpr, i) = "$r($i)" |> rcall
