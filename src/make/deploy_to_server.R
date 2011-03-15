build = TRUE
binary = TRUE
check = FALSE
install = FALSE
source("src/make/make_mlrEDA.R")
source("src/make/make_mlr.R")

wscp = file.path(winscp.path, "winscp.com")

pkg.path = file.path(project.dir, "pkg")

fs1 = sort(list.files(pattern="mlrEDA_.*.zip", path=pkg.path))
fs2 = sort(list.files(pattern="mlrEDA_.*.tar.gz", path=pkg.path))
f1 = file.path(pkg.path, rev(fs1)[1])
f2 = file.path(pkg.path, rev(fs2)[1])
fs3 = sort(list.files(pattern="mlr_.*.zip", path=pkg.path))
fs4 = sort(list.files(pattern="mlr_.*.tar.gz", path=pkg.path))
f3 = file.path(pkg.path, rev(fs3)[1])
f4 = file.path(pkg.path, rev(fs4)[1])


putFile = function(file, remote.path) {
  cmd <- sprintf(
    '/timeout=500 /command 
      "option confirm off" 
      "put %s" "exit"', basename(file)
  )
  cmd  = paste(wscp, " ", compute.login, ":", compute.pwd, "@", compute.server, remote.path, " ", cmd, sep="") 
  message(cmd)
  setwd(dirname(file))
  system(cmd)
  setwd(project.dir)
  message("\nDone.")
}

putFile(f1, "/home/bischl/public_html/win")
putFile(f2, "/home/bischl/public_html/unix")
putFile(f3, "/home/bischl/public_html/win")
putFile(f4, "/home/bischl/public_html/unix")
print(f1)
print(f3)

