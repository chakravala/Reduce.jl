#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

oldwdir = pwd()
wdir = @__DIR__
include("svn.jl")
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
            process = _spawn(`$rpsl`)
            kill(process)
        catch
            if Sys.islinux()
                cmd = `$(joinpath(wdir,"usr","bin"))/$rpsl`
            elseif Sys.isapple()
                cmd = `$(joinpath(wdir,"psl"))/$rpsl`
            else
                cmd = `$(joinpath(wdir,"Reduce-svn$(rsvn[ρ])-src","bin"))/$rpsl`
            end
            process = _spawn(cmd)
            kill(process)
            !isfile("ver") && writever(0)
        end
    catch
        http = "https://sourceforge.net/projects/reduce-algebra/files/snapshot_"
        rtg = "reduce.tar.gz"
        dl = "/download"
        cd(wdir)
        println("Building Reduce.jl with CSL binaries ... ")
        if Sys.islinux()
            src = "/reduce-csl_"
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
            run(`rm -rf $(joinpath(wdir,"csl"))`)
            run(`cp -r /Volumes/$(snap)/csl $(wdir)/csl`)
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
        #cmd = `"$(wdir)\psl\bpsl.exe" -td 16000000 -f "$(wdir)\red\reduce.img"`
        cmd = `"$(wdir)\reduce.exe" --nogui`
        process = _spawn(cmd)
        kill(process)
    catch
        cd(wdir)
        println("Building Reduce.jl with CSL binaries ...")
        cab = "wincsl.cab"
        http = "https://ayera.dl.sourceforge.net/project/reduce-algebra/snapshot_"
        download(http*date[ρ]*"/windows/wincsl_$(rsvn[ρ]).cab",joinpath(wdir,cab))
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
