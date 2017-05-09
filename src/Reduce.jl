module Reduce
using Compat; import Compat.String

#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

immutable PSL <: Base.AbstractPipe
  input::Pipe; output::Pipe; process::Base.Process
  function PSL()
    # Setup pipes and reduce process
    input = Pipe(); output = Pipe()
    process = spawn(`redpsl`, (input, output, STDERR))
    # Close the unneeded ends of Pipes
    close(input.out); close(output.in)
    return new(input, output, process); end; end

Base.kill(rs::PSL) = kill(rs.process)
Base.process_exited(rs::PSL) = process_exited(rs.process)

const EOT = Char(4) # end of transmission character
function Base.write(rs::PSL, input::Compat.String)
  write(rs.input,"$input; symbolic write(string $(Int(EOT)));\n"); end

if VERSION < v"0.5.0"
  function Base.write(rs::PSL, input::UTF8String)
  write(rs.input, "$input; symbolic write(string $(Int(EOT)));\n"); end
  function Base.write(rs::PSL, input::ASCIIString)
  write(rs.input, "$input; symbolic write(string $(Int(EOT)));\n"); end
end

function ReduceCheck(output)
  contains(output,"***** ") && throw(ReduceError(output*"\n")); end

const SOS = "\\n[0-9]+: "
function Base.read(rs::PSL)
  output = String(readuntil(rs.output,EOT))*String(readavailable(rs.output));
  output = replace(output,r"\$\n\n","\n\n");
  output = replace(output,Regex("\\n($EOT$SOS)|($SOS$EOT)"),"")
  ReduceCheck(output); return output; end
function readsp(rs::PSL)
  output = split(read(rs),"\n\n\n")
  for h ∈ 1:length(output)
    output[h] = replace(output[h],Regex(SOS),""); end
  return output; end

include("rexpr.jl")

## io

export string, show
import Base: string, show

string(r::RExpr) = convert(Compat.String,r)
show(io::IO, r::RExpr) = print(io,convert(Compat.String,r))
Base.write(rs::PSL,r::RExpr) = write(rs,convert(Compat.String,r))

@compat function show(io::IO, ::MIME"text/plain", r::RExpr)
  rcall(ra"on nat"); write(rs,r); output = read(rs)
  rcall(ra"off nat"); print(io,replace(output,Regex(SOS),"")); end

@compat function show(io::IO, ::MIME"text/latex", r::RExpr)
  rcall(ra"on latex"); write(rs,r); rd = readsp(rs)
  rcall(ra"off latex"); sp = split(join(rd),"\n\n")
  print(io,"\\begin{eqnarray}\n"); ct = 0
  for str ∈ sp; ct += 1; length(sp) != 1 && print(io,"($ct)\&\\,")
    print(io,join(split(str,"\n")[2:end-1],"\n"));
    ct != length(sp) && print(io,"\\\\\\\\"); end
  print(io,"\n\\end{eqnarray}"); end

## Setup

# Server setup

const rs = PSL()         # Spin up a Reduce session
atexit(() -> kill(rs))      # Kill the session on exit

write(rs,"off nat")
banner = readuntil(rs.output,EOT) |> String; readavailable(rs.output);
ReduceCheck(banner); println(split(String(banner),'\n')[end-3])
ra"load_package rlfi" |> rcall

# REPL setup
#repl_active = isdefined(Base, :active_repl)  # Is an active repl defined?
#interactive = isinteractive()                # In interactive mode?

#if repl_active && interactive
#  repl_init(Base.active_repl)
#end

end # module
