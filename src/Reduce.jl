__precompile__()
module Reduce
using Compat; import Compat.String

#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

include(joinpath(dirname(@__FILE__),"../deps/svn.jl"))

immutable PSL <: Base.AbstractPipe
    input::Pipe
    output::Pipe
    process::Base.Process
    function PSL()
        # Setup pipes and reduce process
        input = Pipe()
        output = Pipe()
        rsl = `$(split(rpsl))`
        dirf = dirname(@__FILE__)
        try
            rsl = `$(split(ENV["REDUCE"]))`
        end
        if !is_windows()
            try
                process = spawn(rsl, (input, output, STDERR))
            catch
                if is_linux()
                    rsl = `$(joinpath(dirf,"..","deps","usr","bin"))/$rpsl`
                elseif is_apple()
                    rsl = `$(joinpath(dirf,"..","deps","psl"))/$rpsl`
                else
                    rsl = `$(joinpath(dirf,"..",rsvn[ρ],"bin"))/$rpsl`
                end
                process = spawn(rsl, (input, output, STDERR))
            end
        else
            dirf = (contains(dirf,"appveyor") ? joinpath(dirf,"..","deps","psl") :
                joinpath(folder,"..","deps","install","lib","psl"))
            rsl = `"$(folder)\psl\bpsl.exe" -td 16000000 -f "$(folder)\red\reduce.img"`
            process = spawn(rsl, (input, output, STDERR))
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

type ReduceError <: Exception
    errstr::Compat.String
end

Base.showerror(io::IO, err::ReduceError) = print(io,"Reduce: "*chomp(err.errstr))

function ReduceCheck(output) # check for REDUCE errors
    contains(output,"***** ")|contains(output,"+++ ") && throw(ReduceError(output))
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
    is_windows() && (out = replace(out,r"\r",""))
    out = replace(replace(out,r"\$\n\n","\n\n"),RES,"")
    out = replace(out,Regex(SOS),"")
    ReduceCheck(out)
    return out
end

readsp(rs::PSL) = split(read(rs),"\n\n\n")

include("rexpr.jl") # load RExpr features
include("parser.jl") # load parser generator
include("repl.jl") # load repl features
include("unary.jl") # load unary operators
include("switch.jl") # load switch operators
include("calculus.jl") # load calculus operators

Base.write(rs::PSL,r::RExpr) = write(rs,convert(Compat.String,r))

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

function Load()
    global rs = PSL()
    s = quote; #global rs = PSL()
        offs = ""
        for o in offlist
            o != :nat && (offs = offs*"off $o; ")
        end
        write(rs.input,"off nat; $EOTstr;\n")
        banner = readuntil(rs.output,EOT) |> String
        readavailable(rs.output)
        is_windows() && (banner = replace(banner,r"\r",""))
        ReduceCheck(banner)
        rcsl = contains(banner," CSL ")
        !(is_windows() && contains(dirname(@__FILE__),"appveyor")) &&
            println(split(String(banner),'\n')[rcsl ? 1 : end-3])
        load_package(:rlfi)
        offs |> RExpr |> rcall
    end
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

end # module
