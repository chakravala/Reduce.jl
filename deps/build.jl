
oldwdir = pwd()
pkgdir = Pkg.dir("Reduce")
wdir = Pkg.dir("Reduce", "deps")

if !is_windows()
   try
     try
       process = spawn(`redpsl`)
       kill(process)
     catch
       cmd = cmd = `$(joinpath("$(Pkg.dir("Reduce", "deps"))","Reduce-svn4052-src","bin"))/redpsl`
       process = spawn(cmd)
       kill(process)
     end
   catch
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
 else
   warn("Windows build of redpsl not currently supported.")
 end

cd(oldwdir)
