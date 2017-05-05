#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

export RExpr, @ra_str, parse, rcall, convert, error, ReduceError, ReduceSyntaxError, ==, getindex

import Base: parse, convert, error, ==, getindex

type ReduceError <: Exception; errstr::Compat.String; end
Base.showerror(io::IO, err::ReduceError) = print(io, err.errstr)
type ReduceSyntaxError <: Exception; errstr::Compat.String; end
Base.showerror(io::IO, err::ReduceSyntaxError) = print(io, err.errstr)

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
  "e"    => "e",
  "i"    => "im",
  "euler_gamma" => "eulergamma",
  "infinity"   =>  "Inf")

const r_to_jl_utf = Dict(
  "pi"   => "π",
  "golden_ratio"  =>  "φ",
  "**"   => "^")
  #"catalan" =>  "catalannum",
  #"khinchin" => "")

const jl_to_r = Dict(
  "e" => "e",
  "eu" => "e",
  "pi" => "pi",
  "eulergamma" => "euler_gamma",
  "golden" => "golden_ratio",
  "im" => "i",
  "Inf" => "infinity")

const jl_to_r_utf = Dict(
  "π" => "pi",
  "γ" => "euler_gamma",
  "ϕ" => "golden_ratio")

function _subst(a, b, expr)
  rstr = "sub(($b) = ($a), ($expr))" |> RExpr
  rstr = rcall(rstr); return rstr.str; end

"""
    RExpr(expr::Expr)
Convert Julia expression to Reduce expression
## Examples
```julia
julia> RExpr(:(sin(x*im) + cos(y*φ)))
                           cos(%phi y) + %i sinh(x)
```
"""
function RExpr(expr::Expr)
  str = unparse(expr)
  for key in keys(jl_to_r_utf)
    str = replace(str,"$(key)",jl_to_r_utf[key]); end
  for key in keys(jl_to_r)
    str = _subst(jl_to_r[key], key, str); end
  RExpr(str); end

"""
    parse(rexpr::RExpr)
Parse a Reduce expression into a Julia expression
## Examples
```julia
julia> parse(ra\"sin(i*x)\")
:(im * sinh(x))
```
"""
function parse(r::RExpr)
  str = r.str
  for key in keys(r_to_jl)
    str = _subst(r_to_jl[key], key, str); end
  for key in keys(r_to_jl_utf)
    str = replace(str,key,r_to_jl_utf[key]); end
  parse(str); end

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
julia> ra\"integrate(sin(x), x)\"
                             integrate(sin(x), x)
julia> rcall(ans)
                                   - cos(x)
```
"""
function rcall(r::RExpr)
  write(rs, r.str); sleep(slp); output = read(rs)
  return RExpr(split(output,'\n')[end]); end

"""
	rcall{T}(expr::T)
Evaluate a Julia expression or string using the Reduce interpretor and convert
output back into the input type
## Examples
```julia
julia> rcall(\"integrate(sin(y)^2, y)\")
\"(y-sin(2*y)/2)/2\"
julia> rcall(:(integrate(1/(1+x^2), x)))
:(atan(x))
```
"""
rcall{T}(expr::T) = convert(T, rcall(RExpr(expr)))

function ==(r::RExpr, s::RExpr)
  return rcall(RExpr("if ($r) = ($s) then 1 else 0")) |> parse |> eval; end
getindex(r::RExpr, i) = (return RExpr("$r($i)") |> rcall)
