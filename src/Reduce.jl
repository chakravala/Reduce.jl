__precompile__()
module Reduce
using Compat; import Compat.String

#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

include(joinpath(dirname(@__FILE__),"../deps/svn.jl"))

immutable PSL <: Base.AbstractPipe
  input::Pipe; output::Pipe; process::Base.Process
  function PSL()
    if !is_windows()
      try
        # Setup pipes and reduce process
        input = Pipe(); output = Pipe()
        process = spawn(`$rpsl`, (input, output, STDERR))
        # Close the unneeded ends of Pipes
        close(input.out); close(output.in)
        return new(input, output, process)
      catch
        # Setup pipes and reduce process
        input = Pipe(); output = Pipe()
        if is_linux()
          cmd = `$(joinpath(dirname(@__FILE__),"..","deps","usr","bin"))/$rpsl`
        elseif is_apple()
          cmd = `$(joinpath(dirname(@__FILE__),"..","deps","psl"))/$rpsl`
        else
          cmd = `$(joinpath(dirname(@__FILE__),"..",rsvn[ρ],"bin"))/$rpsl`
        end
        process = spawn(cmd, (input, output, STDERR))
        # Close the unneeded ends of Pipes
        close(input.out); close(output.in)
        return new(input, output, process)
      end
    else
      # Setup pipes and reduce process
      input = Pipe(); output = Pipe()
      folder = joinpath(dirname(@__FILE__)," ..","deps","install","lib","psl")
      cmd = `"$(folder)\psl\pbsl.exe" -td 16000000 -f "$(folder)\red\reduce.img"`
      process = spawn(cmd, (input, output, STDERR))
      # Close the unneeded ends of Pipes
      close(input.out); close(output.in)
      return new(input, output, process)
    end
  end; end

Base.kill(rs::PSL) = kill(rs.process)
Base.process_exited(rs::PSL) = process_exited(rs.process)

const EOT = Char(4) # end of transmission character
function Base.write(rs::PSL, input::Compat.String)
  write(rs.input,"$input; symbolic write(string $(Int(EOT)));\n"); end

if VERSION < v"0.5.0" # backwards compatability
  function Base.write(rs::PSL, input::UTF8String)
  write(rs.input,"$input; symbolic write(string $(Int(EOT)));\n"); end
  function Base.write(rs::PSL, input::ASCIIString)
  write(rs.input,"$input; symbolic write(string $(Int(EOT)));\n"); end
end

function ReduceCheck(output) # check for REDUCE errors
  contains(output,"***** ") && throw(ReduceError(output*"\n")); end

const SOS = "[0-9]+: " # REDUCE terminal prompt
function Base.read(rs::PSL) # get result and strip prompts/EOT char
  out = String(readuntil(rs.output,EOT))*String(readavailable(rs.output));
  out = replace(out,r"\$\n\n","\n\n")
  out = replace(out,Regex("\n($EOT\n$SOS)|(\n$SOS\n$EOT)"),"")
  out = replace(out,Regex(SOS),""); ReduceCheck(out); return out; end
readsp(rs::PSL) = split(read(rs),"\n\n\n")

include("rexpr.jl") # load RExpr features

## io

export string, show, ResetReduce
import Base: string, show

string(r::RExpr) = convert(Compat.String,r)
show(io::IO, r::RExpr) = print(io,convert(Compat.String,r))
Base.write(rs::PSL,r::RExpr) = write(rs,convert(Compat.String,r))

@compat function show(io::IO, ::MIME"text/plain", r::RExpr)
  rcall(ra"on nat"); write(rs,r); output = read(rs)
  rcall(ra"off nat"); print(io,replace(output,Regex("\n"*SOS),"")); end

@compat function show(io::IO, ::MIME"text/latex", r::RExpr)
  rcall(ra"on latex"); write(rs,r); rd = readsp(rs)
  rcall(ra"off latex"); sp = split(join(rd),"\n\n")
  print(io,"\\begin{eqnarray}\n"); ct = 0 # add enumeration
  for str ∈ sp; ct += 1; length(sp) ≠ 1 && print(io,"($ct)\&\\,")
    print(io,replace(str,r"(\\begin{displaymath})|(\\end{displaymath})","")) # strip LaTeX
    ct ≠ length(sp) && print(io,"\\\\\\\\"); end # new line
  print(io,"\n\\end{eqnarray}"); end

include("repl.jl") # load repl features

## Setup

"""
  ResetReduce()
Kills the REDUCE process and starts a new instance.
## Examples
```julia
julia> ResetReduce()
Reduce (Free PSL version, revision 4015),  5-May-2017 ...
```
"""
ResetReduce() = (kill(rs); LoadReduce())
__init__() = (LoadReduce(); atexit(() -> kill(rs)))

# Server setup

function LoadReduce()
  global rs = PSL(); write(rs,"off nat") # disable nat mode
  banner = readuntil(rs.output,EOT) |> String; readavailable(rs.output);
  ReduceCheck(banner); println(split(String(banner),'\n')[end-3])
  ra"load_package rlfi" |> rcall # load REDUCE's LaTeX package
  if isdefined(Base,:active_repl) && isinteractive()
    repl_init(Base.active_repl); end; end

end # module
