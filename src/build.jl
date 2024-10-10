#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

oldwdir = pwd()
wdir = joinpath(@__DIR__,"../deps")

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

if isfile("ver")
    global ver = NaN
    open("ver","r") do f
        global ver = parse(Int,read(f,String))
    end
    if ver ≠ ρ
        if Sys.islinux()
            run(`rm -rf $(joinpath(wdir,"usr"))`)
        elseif Sys.isapple()
            run(`rm -rf $(joinpath(wdir,"csl"))`)
        elseif Sys.iswindows()
            run(`cmd /C DEL /F /S /Q /A "$(joinpath(wdir,"reduce.exe"))"`)
        end
        run(Sys.iswindows() ? `cmd /C DEL /F /S /Q /A "ver"` : `rm ver`)
    end
end

function writever(rv)
    open("ver","w") do f
        write(f,"$rv")
    end
end

if !Sys.iswindows()
    try
        try
            process = _spawn(red())
            kill(process)
        catch
            process = _spawn(redsys(wdir))
            kill(process)
            !isfile("ver") && writever(0)
        end
    catch
        http = "https://sourceforge.net/projects/reduce-algebra/files/snapshot_"
        rtg = "reduce.tar.gz"
        dl = "/download"
        cd(wdir)
        println("Building Reduce.jl with $(ispsl() ? "PSL" : "CSL") binaries ... ")
        if Sys.islinux()
            src = "/reduce-$(redsl())_"
            if occursin("64",read(`uname -m`,String))
                download(http*date[ρ]*"/linux64"*src*rsvn[ρ]*"_amd64.tgz"*dl,joinpath(wdir,rtg))
            else
                download(http*date[ρ]*"/linux32"*src*rsvn[ρ]*"_i386.tgz"*dl,joinpath(wdir,rtg))
            end
            run(`rm -rf $(joinpath(wdir,"usr"))`)
            run(`tar -xvf $(rtg)`)
            run(`rm $(rtg)`)
            writever(ρ)
        elseif Sys.isapple()
            snap = "Reduce-snapshot"
            download(http*date[ρ]*"/macintosh/"*snap*"_"*rsvn[ρ]*".dmg"*dl,joinpath(wdir,"$(snap)_$(date[ρ]).dmg"))
            run(`hdiutil attach $(wdir)/$(snap)_$(date[ρ]).dmg`)
            run(`rm -rf $(joinpath(wdir,redsl()))`)
            run(`cp -r /Volumes/$(snap)/csl $(wdir)/$(redsl())`)
            run(`hdiutil unmount /Volumes/$(snap)`)
            run(`rm $(snap)_$(date[ρ]).dmg`)
            writever(ρ)
        else
            download(http*date[ρ]*"/Reduce-svn$(rsvn[ρ])-src.tar.gz"*dl,joinpath(wdir,rtg))
            run(`tar -xvf $(rtg)`)
            run(`rm $(rtg)`)
            cd(joinpath("$wdir","Reduce-svn$(rsvn[ρ])-src"))
            run(`./configure --with-psl`)
            run(`make`)
            writever(ρ)
        end
        println("DONE")
    end
else
    try
        process = _spawn(redsys(wdir))
        kill(process)
    catch
        cd(wdir)
        println("Building Reduce.jl with $(ispsl() ? "PSL" : "CSL") binaries ...")
        cab,winsl = "winred.cab",ispsl() ? "winpsl64" : "wincsl"
        http = "https://master.dl.sourceforge.net/project/reduce-algebra/snapshot_"
        download(http*date[ρ]*"/windows/$(winsl)_$(rsvn[ρ]).cab",joinpath(wdir,cab))
        open("redextract.bat","w") do f
                write(f,"expand $cab \"$(wdir)\" -F:*\n")
                #write(f,"reg Query \"HKLM\\Hardware\\Description\\System\\CentralProcessor\\0\" | find /i \"x86\" > NUL && set OSQ=32BIT || set OSQ=64BIT\n")
                #write(f,"echo %OSQ% > osbit.txt")
        end
        run(`$(wdir)\\redextract.bat`)
        writever(ρ)
        println("DONE")
    end
end

cd(oldwdir)
