#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

oldwdir = pwd()
wdir = dirname(@__FILE__)
date = "2017-05-16"
rsvn = "Reduce-svn4052-src"
rpsl = "redpsl"

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
         cmd = `$(joinpath(wdir,rsvn,"bin"))/$rpsl`
       end
       process = spawn(cmd)
       kill(process)
     end
   catch
     http = "https://sourceforge.net/projects/reduce-algebra/files/snapshot_"
     src = "/linux-tar/reduce-psl_"
     dl = "/download"
     cd(wdir)
     if is_linux()
       if contains(readstring(`uname -m`),"64")
         download(http*date*src*date*"_amd64.tgz"*dl,joinpath(wdir,"reduce.tar.gz"))
       else
         download(http*date*src*date*"_i386.tgz"*dl,joinpath(wdir,"reduce.tar.gz"))
       end
       println("Building redpsl ... ")
       run(`tar -xvf reduce.tar.gz`)
       run(`rm reduce.tar.gz`)
     elseif is_apple()
       snap = "Reduce-snapshot"
       download(http*date*"/"*snap*"_"*date*".dmg"*dl,joinpath(wdir,"$(snap)_$(date).dmg"))
       println("Building redpsl")
       run(`hdiutil attach $(wdir)/$(snap)_$(date).dmg`)
       run(`cp -r /Volumes/$(snap)/psl $(wdir)/psl`)
       run(`hdiutil unmount /Volumes/$(snap)`)
       run(`rm $(snap)_$(date).dmg`)
     else
       download(http*date*"/"*rsvn*".tar.gz"*dl,joinpath(wdir,"reduce.tar.gz"))
       println("Building redpsl ... ")
       run(`tar -xvf reduce.tar.gz`)
       run(`rm reduce.tar.gz`)
       cd(joinpath("$wdir",rsvn))
       run(`./configure --with-psl`)
       run(`make`)
     end
     println("DONE")
   end
 else
   warn("Windows build of redpsl not currently supported.")
 end

cd(oldwdir)
