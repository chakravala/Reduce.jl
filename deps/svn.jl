#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

ispsl() = haskey(ENV,"PSL")
red() = ispsl() ? `redpsl` : `redcsl -w`
red(path) = ispsl() ? `$path/redpsl` : `$path/redcsl -w`
redsl() = ispsl() ? "psl" : "csl"

ρ = 7

date = Dict(
  0 =>  "2017-05-16",
  1 =>  "2018-01-17",
  2 =>  "2018-04-06",
  3 =>  "2019-04-13",
  4 =>  "2019-09-15",
  5 =>  "2020-03-01",
  6 =>  "2020-06-24",
  7 =>  "2020-10-07")

rsvn = Dict(
  0 =>  "4052",
  1 =>  "4372",
  2 =>  "4567",
  3 =>  "4961",
  4 =>  "5129",
  5 =>  "5286",
  6 =>  "5377",
  7 =>  "5424")

function redsys(dir)
    if !(Sys.iswindows())
        if Sys.islinux()
            red(joinpath(dir,"usr","bin"))
        elseif Sys.isapple()
            red(joinpath(dir,ispsl() ? "psl" : "csl"))
        else
            rsl = red(joinpath(dir,"Reduce-svn$(rsvn[ρ])-src","bin"))
        end
    else
        if ispsl()
            `"$(dir)\psl\bpsl.exe" -td 16000000 -f "$(dir)\red\reduce.img"`
        else
            `"$(dir)\reduce.exe" --nogui`
        end
    end
end

function _spawn(cmd, input=devnull, output=devnull)
    run(pipeline(cmd, stdin=input, stdout=output, stderr=stderr), wait=false)
end
