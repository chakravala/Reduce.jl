#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

rpsl = "redpsl"

ρ = 1 # default # version history

date = Dict(
  0 =>  "2017-05-16",
  1 =>  "2018-01-17")

rsvn = Dict(
  0 =>  "Reduce-svn4052-src",
  1 =>  "Reduce-svn4372-src")

if VERSION >= v"0.7.0-DEV.4445"
    function _spawn(cmd, input=devnull, output=devnull)
        run(pipeline(cmd, stdin=input, stdout=output, stderr=stderr), wait=false)
    end
else
    _spawn(cmd, input=DevNull, output=DevNull) = spawn(cmd, (input, output, STDERR))
end
