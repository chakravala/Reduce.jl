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
  end; end

Base.kill(rs::PSL) = kill(rs.process)
Base.process_exited(rs::PSL) = process_exited(rs.process)

clear(rs::PSL) = (write(rs.input,";\n"); readavailable(rs.output))
clears = (()->(c=true; return (tf=c)->(c≠tf && (c=tf); return c)))()

const EOT = Char(4) # end of transmission character
EOTstr = "symbolic write(string $(Int(EOT)))"

function Base.write(rs::PSL, input::Compat.String)
    clears() && clear(rs)
    write(rs.input,"$input; $EOTstr;\n")
end

function ReduceCheck(output) # check for REDUCE errors
    contains(output,"***** ") && throw(ReduceError(output))
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

## io

export string, show, load_package
import Base: string, show

string(r::RExpr) = convert(Compat.String,r)
show(io::IO, r::RExpr) = print(io,convert(Compat.String,r))
Base.write(rs::PSL,r::RExpr) = write(rs,convert(Compat.String,r))

@compat function show(io::IO, ::MIME"text/plain", r::RExpr)
    length(r.str) > 1 && (show(string(r)); return nothing)
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
        length(sp) ≠ 1 && print(io,"($ct)\&\\,")
        print(io,replace(str,r"(\\begin{displaymath})|(\\end{displaymath})",""))
        ct ≠ length(sp) && print(io,"\\\\\\\\")
    end # new line
    print(io,"\n\\end{eqnarray}")
end

include("repl.jl") # load repl features
include("unary.jl") # load unary operators
include("switch.jl") # load switch operators

## Setup

const offlist = [:nat,:latex,:exp]

"""
    load_package(::Symbol)

Loads the specified package into REDUCE

# Examples
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

"""
    Reduce.Reset()

Kills the REDUCE process and starts a new instance.

# Examples
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
        !(is_windows() && contains(dirname(@__FILE__),"appveyor")) &&
            println(split(String(banner),'\n')[end-3])
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
