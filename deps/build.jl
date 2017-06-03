#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

oldwdir = pwd()
wdir = dirname(@__FILE__)
include("svn.jl")

if !is_windows()
  try
    try
      process = spawn(`$rpsl`)
      kill(process)
    catch
      if is_linux()
        cmd = `$(joinpath(wdir,"usr","bin"))/$rpsl`
      elseif is_apple()
        cmd = `$(joinpath(wdir,"psl"))/$rpsl`
      else
        cmd = `$(joinpath(wdir,rsvn[ρ],"bin"))/$rpsl`
      end
      process = spawn(cmd)
      kill(process)
    end
  catch
    http = "https://sourceforge.net/projects/reduce-algebra/files/snapshot_"
    rtg = "reduce.tar.gz"
    dl = "/download"
    cd(wdir)
    println("Building redpsl ... ")
    if is_linux()
      src = "/linux-tar/reduce-psl_"
      if contains(readstring(`uname -m`),"64")
        download(http*date[ρ]*src*date[ρ]*"_amd64.tgz"*dl,joinpath(wdir,rtg))
      else
        download(http*date[ρ]*src*date[ρ]*"_i386.tgz"*dl,joinpath(wdir,rtg))
      end
      run(`rm -rf $(joinpath(wdir,"usr"))`)
      run(`tar -xvf $(rtg)`)
      run(`rm $(rtg)`)
    elseif is_apple()
      snap = "Reduce-snapshot"
      download(http*date[ρ]*"/"*snap*"_"*date[ρ]*".dmg"*dl,joinpath(wdir,"$(snap)_$(date[ρ]).dmg"))
      run(`hdiutil attach $(wdir)/$(snap)_$(date[ρ]).dmg`)
      run(`rm -rf $(joinpath(wdir,"psl"))`)
      run(`cp -r /Volumes/$(snap)/psl $(wdir)/psl`)
      run(`hdiutil unmount /Volumes/$(snap)`)
      run(`rm $(snap)_$(date[ρ]).dmg`)
    else
      download(http*date[ρ]*"/"*rsvn[ρ]*".tar.gz"*dl,joinpath(wdir,rtg))
      run(`tar -xvf $(rtg)`)
      run(`rm $(rtg)`)
      cd(joinpath("$wdir",rsvn[ρ]))
      run(`./configure --with-psl`)
      run(`make`)
    end
    println("DONE")
  end
else
  try
    folder = joinpath(dirname(@__FILE__)," ..","deps","install","lib","psl")
    cmd = `"$(folder)\psl\pbsl.exe" -td 16000000 -f "$(folder)\red\reduce.img"`
    process = spawn(cmd)
    kill(process)
  catch
    cd(wdir)
    http = "https://ayera.dl.sourceforge.net/project/reduce-algebra/snapshot_"
    setup = "Reduce-Setup"
    if contains(wdir,"appveyor")
      println("Building redpsl...")
      download("https://github.com/chakravala/Reduce.jl/wiki/redpsl.cab",joinpath(wdir,"redpsl.cab"))
      open("redpsl.bat","w") do f
        write(f,"expand redpsl.cab \"$(wdir)\" -F:*"); end
      run(`$(wdir)\\redpsl.bat`)
    else
      println("Downloading reduce binaries...")
      download(http*date[ρ]*"/"*setup*"_"*date[ρ]*".exe",joinpath(wdir,"$(setup)_$(date[ρ]).exe"))
      println("Installing reduce binaries...")
      run(`$(setup)_$(date[ρ]).exe /SILENT /DIR=$(joinpath(wdir,"install"))`)
    end
    println("DONE")
  end
end

cd(oldwdir)
