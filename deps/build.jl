#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

oldwdir = pwd()
wdir = dirname(@__FILE__)
rpsl = "redpsl"
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
         cmd = `$(joinpath(wdir,rsvn[0],"bin"))/$rpsl`
       end
       process = spawn(cmd)
       kill(process)
     end
   catch
     http = "https://sourceforge.net/projects/reduce-algebra/files/snapshot_"
     src = "/linux-tar/reduce-psl_"
     dl = "/download"
     rtg = "reduce.tar.gz"
     cd(wdir)
     println("Building redpsl ... ")
     if is_linux()
       if contains(readstring(`uname -m`),"64")
         download(http*date[0]*src*date[0]*"_amd64.tgz"*dl,joinpath(wdir,rtg))
       else
         download(http*date[0]*src*date[0]*"_i386.tgz"*dl,joinpath(wdir,rtg))
       end
       run(`rm -rf $(joinpath(wdir,"usr"))`)
       run(`tar -xvf $(rtg)`)
       run(`rm $(rtg)`)
     elseif is_apple()
       snap = "Reduce-snapshot"
       download(http*date[0]*"/"*snap*"_"*date[0]*".dmg"*dl,joinpath(wdir,"$(snap)_$(date[0]).dmg"))
       run(`hdiutil attach $(wdir)/$(snap)_$(date[0]).dmg`)
       run(`rm -rf $(joinpath(wdir,"psl"))`)
       run(`cp -r /Volumes/$(snap)/psl $(wdir)/psl`)
       run(`hdiutil unmount /Volumes/$(snap)`)
       run(`rm $(snap)_$(date[0]).dmg`)
     else
       download(http*date[0]*"/"*rsvn[0]*".tar.gz"*dl,joinpath(wdir,rtg))
       run(`tar -xvf $(rtg)`)
       run(`rm $(rtg)`)
       cd(joinpath("$wdir",rsvn[0]))
       run(`./configure --with-psl`)
       run(`make`)
     end
     println("DONE")
   end
else
   #= try
     cmd = `cmd \c %programfiles%\\Reduce\\bin\\redpsl.bat`
     process = spawn(cmd)
     kill(process)
   catch
     cd(wdir)
     setup = "Reduce-Setup"
     download(http*date[0]*"/"*setup*"_"*date[0]*".exe"*dl,joinpath(wdir,"$(setup)_$(date[0]).exe"))
     run(`cmd \c $(setup)_$(date[0]).exe`)
     run(`cmd \c DEL $(setup)_$(date[0]).exe`)
     println("DONE")
   end =#
   warn("Windows build of redpsl not currently supported.")
 end

cd(oldwdir)
