#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

export RExpr, @ra_str, parse, rcall, convert, error, ReduceError, ==, getindex

import Base: parse, convert, error, ==, getindex

type ReduceError <: Exception; errstr::Compat.String; end
Base.showerror(io::IO, err::ReduceError) = print(io,"REDUCE: "*err.errstr)

const infix_ops = [:+, :-, :*, :/, :^]
isinfix(args) = args[1] in infix_ops && length(args) > 2
show_expr(io::IO, ex) = print(io, ex)

function show_expr(io::IO, expr::Expr)
  if expr.head != :call; error("Block structure is not supported by Reduce.jl")
  else; isinfix(expr.args) ? print(io,"$(expr.args[1])") : show_expr(io, expr.args[1])
    print(io, "("); args = expr.args[2:end]; for (i, arg) in enumerate(args)
      show_expr(io, arg); i!=endof(args) ? print(io,","):print(io,")"); end; end; end
unparse(expr::Expr) = (io = IOBuffer(); show_expr(io, expr); return String(io))

"""
A Reduce expression
## Summary:
type RExpr <: Any
## Fields:
str :: String
"""
type RExpr; str::Compat.String; end
macro ra_str(str); RExpr(str); end

const r_to_jl = Dict(
  "i"             =>  "im",
  "euler_gamma"   =>  "eulergamma",
  "infinity"      =>  "Inf")

const r_to_jl_utf = Dict(
  "pi"            =>  "π",
  "golden_ratio"  =>  "φ",
  "**"            =>  "^")

const jl_to_r = Dict(
  "eu"            =>  "e",
  "eulergamma"    =>  "euler_gamma",
  "golden"        =>  "golden_ratio",
  "im"            =>  "i",
  "Inf"           =>  "infinity")

const jl_to_r_utf = Dict(
  "π"             =>  "pi",
  "γ"             =>  "euler_gamma",
  "ϕ"             =>  "golden_ratio",
  "^"             =>  "**")

function _syme(syme::Dict{String,String}); str = ""; for key in keys(syme)
  str = str*"($key)=($(syme[key])),"; end; return str[1:end-1]; end

const symrjl = _syme(r_to_jl)
const symjlr = _syme(jl_to_r)

_subst(syme::String,expr) = "sub({$syme},($expr))" |> RExpr |> rcall

"""
  RExpr(expr::Expr)
Convert Julia expression to Reduce expression
## Examples
```julia
julia> rcall(:(sin(x*im) + cos(y*ϕ)))
:(cos((sqrt(5) * y + y) / 2) + sinh(x) * im)
```
"""
function RExpr(expr::Expr)
  str = unparse(expr)
  for key in keys(jl_to_r_utf)
    str = replace(str,key,jl_to_r_utf[key]); end
  return _subst(symjlr,str); end

"""
  parse(rexpr::RExpr)
Parse a Reduce expression into a Julia expression
## Examples
```julia
julia> parse(ra\"sin(i*x)\")
:(sinh(x) * im)
```
"""
function parse(r::RExpr)
  str = _subst(symrjl,r.str).str
  for key in keys(r_to_jl_utf)
    str = replace(str,key,r_to_jl_utf[key]); end
  return parse(str); end

convert(::Type{Compat.String}, r::RExpr) = r.str
convert(::Type{Expr}, r::RExpr) = parse(r)
if VERSION < v"0.5.0"
  convert(::Type{UTF8String}, r::RExpr) = UTF8String(r.str)
  convert(::Type{ASCIIString}, r::RExpr) = ASCIIString(r.str)
end

"""
  rcall(r::RExpr)
Evaluate a Reduce expression.
## Examples
```julia
julia> ra\"int(sin(x), x)\"
 - cos(x)
julia> rcall(ans)
 - cos(x)
```
"""
rcall(r::RExpr) = (write(rs, r.str); return replace(read(rs),r"\n","") |> RExpr)

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
  return "if ($r) = ($s) then 1 else 0" |> rcall |> parse |> eval |> Bool; end
getindex(r::RExpr, i) = "$r($i)" |> rcall
