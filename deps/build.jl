#   This file is part of Reduce.jl. It is licensed under the MIT license
#   Copyright (C) 2017 Michael Reed

oldwdir = pwd()
wdir = dirname(@__FILE__)

if !is_windows()
   try
     try
       process = spawn(`redpsl`)
       kill(process)
     catch
       if is_linux()
         cmd = `$(joinpath(wdir,"usr","bin"))/redpsl`
       else
         cmd = `$(joinpath(wdir,"Reduce-svn4052-src","bin"))/redpsl`
       end
       process = spawn(cmd)
       kill(process)
     end
   catch
     if is_linux()
       if contains(readstring(`uname -m`),"64")
         download("https://sourceforge.net/projects/reduce-algebra/files/snapshot_2017-05-16/linux-tar/reduce-psl_2017-05-16_amd64.tgz/download",joinpath(wdir,"reduce.tar.gz"))
       else
         download("https://sourceforge.net/projects/reduce-algebra/files/snapshot_2017-05-16/linux-tar/reduce-psl_2017-05-16_i386.tgz/download",joinpath(wdir,"reduce.tar.gz"))
       end
       cd(wdir)
       println("Building redpsl ... ")
       run(`tar -xvf reduce.tar.gz`)
       run(`rm reduce.tar.gz`)
       println("DONE")
     else
       download("https://sourceforge.net/projects/reduce-algebra/files/snapshot_2017-05-16/Reduce-svn4052-src.tar.gz/download",joinpath(wdir,"reduce.tar.gz"))
       cd(wdir)
       println("Building redpsl ... ")
       run(`tar -xvf reduce.tar.gz`)
       run(`rm reduce.tar.gz`)
       cd(joinpath("$wdir","Reduce-svn4052-src"))
       run(`./configure --with-psl`)
       run(`make`)
       println("DONE")
     end
   end
 else
   warn("Windows build of redpsl not currently supported.")
 end

cd(oldwdir)
