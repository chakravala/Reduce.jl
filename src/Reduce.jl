__precompile__()
module Reduce
using ForceImport
using Compat; import Compat.String

#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

include(joinpath(@__DIR__,"../deps/svn.jl"))

struct PSL <: Base.AbstractPipe
    input::Pipe
    output::Pipe
    process::Base.Process
    function PSL()
        # Setup pipes and reduce process
        input = Pipe()
        output = Pipe()
        rsl = `$(split(rpsl))`
        dirf = @__DIR__
        if !is_windows()
            try
                process = _spawn(rsl, input, output)
            catch
                if is_linux()
                    rsl = `$(joinpath(dirf,"..","deps","usr","bin"))/$rpsl`
                elseif is_apple()
                    rsl = `$(joinpath(dirf,"..","deps","csl"))/$rpsl -w`
                else
                    rsl = `$(joinpath(dirf,"..","Reduce-svn$(rsvn[ρ])-src","bin"))/$rpsl`
                end
                process = _spawn(rsl, input, output)
            end
        else
            dirf = joinpath(dirf,"..","deps")
            #rsl = `"$(dirf)\psl\bpsl.exe" -td 16000000 -f "$(dirf)\red\reduce.img"`
            rsl = `"$(dirf)\reduce.exe" --nogui`
            process = _spawn(rsl, input, output)
        end
        # Close the unneeded ends of Pipes
        close(input.out)
        close(output.in)
        return new(input, output, process)
    end
end

Base.kill(rs::PSL) = kill(rs.process)
Base.process_exited(rs::PSL) = process_exited(rs.process)

export error, ReduceError
import Base: error

struct ReduceError <: Exception
    errstr::Compat.String
end

Base.showerror(io::IO, err::ReduceError) = print(io,"Reduce: "*chomp(err.errstr))

function ReduceCheck(output) # check for REDUCE errors
    contains(output,r"(([*]{5})|([+]{3}) )|( ?  \(Y or N\))") && throw(ReduceError(output))
end

function ReduceWarn(output) # check for REDUCE warnings
    if contains(output,r"[*]{3}")
        info("REDUCE: "*chomp(output))
        join(split(output,r"[*]{3}.*\n"))
    else
        output
    end
end

function PipeClogged(tf::Bool,c::Int,info::String)
    warn("Reduce pipe clogged by $info, $(tf ? "success" : "failure") after $c tries")
end

clear(rs::PSL) = (write(rs.input,";\n"); readavailable(rs.output))
clears = (()->(c=true; return (tf=c)->(c≠tf && (c=tf); return c)))()

const EOT = Char(4) # end of transmission character
EOTstr = "symbolic write(int2id $(Int(EOT)))"

function Base.write(rs::PSL, input::Compat.String)
    clears() && clear(rs)
    write(rs.input,"$input; $EOTstr;\n")
end

const SOS = "[0-9]+: " # REDUCE terminal prompt
const RES = Regex("\n($EOT\n$SOS)|(\n$SOS\n$EOT)|(\n$SOS$EOT\n)|($EOT\n)")

function Base.read(rs::PSL) # get result and strip prompts/EOT char
    out = String(readuntil(rs.output,EOT))*String(readavailable(rs.output))
    is_windows() && (out = replace(out,r"\r" => ""))
    out = replace(replace(out,r"\$\n\n" => "\n\n"),RES=>"")
    out = replace(out,Regex(SOS) => "")
    ReduceCheck(out)
    return ReduceWarn(out)
end

readsp(rs::PSL) = split(read(rs),"\n\n\n")

include("rexpr.jl") # load RExpr features
include("parser.jl") # load parser generator
include("repl.jl") # load repl features
include("switch.jl") # load switch operators

module Algebra
importall Reduce
using Compat
import Compat.String

include("unary.jl") # load unary operators
include("args.jl") # load calculus operators

const variables = [
    :root_multiplicities,
    :requirements,
    :assumptions,
    :low_pow,
    :high_pow,
    :euler_gamma,
    :khinchin,
]

for var ∈ [variables;[:ws]]
    :($var() = rcall(RExpr($(string(var)))) |> parse) |> eval
end

@doc """
    root_multiplicities()

Solution multiplicities are stored in the global variable `root_multiplicities` rather than the solution list. The value of this variable is a list of the multiplicities of the solutions for the last call of `solve`. For example,
```Julia
julia> Algebra.solve(:(x^2==2x-1),:x); Algebra.root_multiplicities()
```
gives the results
```Julia
(:(x = 1),)
 
(2,)
```
""" Reduce.Algebra.root_multiplicities

@doc """
    requirements()

The proper design of a variable sequence supplied as a second argument to `solve` is important for the structure of the solution of an equation system. Any unknown in the system not in this list is considered totally free. E.g. the call
```Julia
Algebra.solve((:(x==2z),:(z==2y)),(:z,))
```
produces an empty list as a result because there is no function `z = z(x,y)` which fulfills both equations for arbitrary `x` and `y` values. In such a case the share variable `requirements` displays a set of restrictions for the parameters of the system:

```Julia
julia> Algebra.requirements()
(:(x - 4y),)
```

The non-existence of a formal solution is caused by a contradiction which disappears only if the parameters of the initial system are set such that all members of the requirements list take the value zero. For a linear system the set is complete: a solution of the requirements list makes the initial system solvable. E.g. in the above case a substitution ``x = 4y`` makes the equation set consistent. For a non-linear system only one inconsistency is detected. If such a system has more than one inconsistency, you must reduce them one after the other. 1 The set shows you also the dependency among the parameters: here one of ``x`` and ``y`` is free and a formal solution of the system can be computed by adding it to the variable list of `solve`. The requirement set is not unique – there may be other such sets.
""" Reduce.Algebra.requirements

@doc """
    assumptions()

A system with parameters may have a formal solution, e.g. 
```Julia
julia> Algebra.solve((:(x==a*z+1),:(0==b*z-y)),(:z,:x))
(:(z = y // b), :(x = (a * y + b) // b))
```
which is not valid for all possible values of the parameters. The variable `assumptions` contains then a list of restrictions: the solutions are valid only as long as none of these expressions vanishes. Any zero of one of them represents a special case that is not covered by the formal solution. In the above case the value is
```Julia
julia> Algebra.assumptions()
(:b,)
```
which excludes formally the case ``b = 0``; obviously this special parameter value makes the system singular. The set of assumptions is complete for both, linear and non–linear systems.
""" Reduce.Algebra.assumptions

@doc """
    euler_gamma()

Euler's constant, also available as -\$\\psi(1)\$.
""" Reduce.Algebra.euler_gamma

@doc """
    khinchin()

Khinchin's constant, defined as

\$ \\prod_{n=1}^\\infty \\left( 1 + \\frac{1}{n(n+2)} \\right)^{\\log_2 n}. \$
""" Reduce.Algebra.khinchin

end

export Algebra, @force

Base.write(rs::PSL,r::RExpr) = write(rs,convert(Compat.String,r))

import Base: zero, one

for T ∈ [:Any,:Expr,:Symbol]
    @eval begin
        zero(::Type{$T}) = 0
        zero(::$T) = 0
        one(::Type{$T}) = 1
        one(::$T) = 1
    end
end

import Base.LinAlg: transpose, ctranspose

transpose(r::ExprSymbol) = r
ctranspose(r::ExprSymbol) = Algebra.conj(r)

## Setup

const offlist = [:nat,:latex,:exp]

export load_package, @load_package

"""
    load_package(::Symbol)

Loads the specified package into REDUCE

## Examples
```julia-repl
julia> load_package(:rlfi)
```
"""
function load_package(pkg::Union{String,Symbol},pkgs...)
    "load_package $pkg" |> rcall
    for extra in pkgs
        load_package(extra)
    end
    return nothing
end
function load_package(pkgs::Union{Array{String,1},Array{Symbol,1}})
    for pkg in pkgs
        load_package(pkg)
    end
end

macro load_package(pkg...)
    load_package(pkg...)
end

"""
    Reduce.Reset()

Kills the REDUCE process and starts a new instance.

## Examples
```julia-repl
julia> Reduce.Reset()
Reduce (Free PSL version, revision 4015),  5-May-2017 ...
```
"""
Reset() = (kill(rs); Load())
__init__() = (Load(); atexit(() -> kill(rs)))

# Server setup

const s = quote; #global rs = PSL()
    global offs = ""
    for o in offlist
        o != :nat && (offs = offs*"off $o; ")
    end
    write(rs.input,"off nat; $EOTstr;\n")
    banner = readuntil(rs.output,EOT) |> String
    readavailable(rs.output)
    rcsl = contains(banner," CSL ")
    if is_windows()
        banner = replace(banner,r"\r" => "")
        println(split(String(banner),'\n')[rcsl ? 1 : end-3])
        ColCheck(false)
    else
        ReduceCheck(banner)
        println(split(String(banner),'\n')[rcsl ? 1 : end-3])
    end
    load_package(:rlfi)
    offs |> RExpr |> rcall
    rcall(R"on savestructr")
    show(DevNull,"text/latex",R"int(sinh(e**i*z),z)")
    R"x" == R"x"
    ListPrint(0)
end

function Load()
    global rs = PSL()
    global s
    if isdefined(Base,:active_repl) && isinteractive()
        eval(s)
        repl_init(Base.active_repl)
    elseif isdefined(Main,:IJulia)
        eval(s)
    else
        atreplinit() do repl
            eval(s)
            !isdefined(Main,:OhMyREPL) &&
                (repl.interface = Base.REPL.setup_interface(repl))
            repl_init(Base.active_repl)
            print('\n')
        end
    end
    return nothing
end

global preload = false
try
    (ENV["REDPRE"] == "1") && (preload = true)
end
preload && include("precomp.jl")

end # module
