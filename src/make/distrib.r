source("src/make/config.r")

rev <- 400
wscp <- file.path(winscp.path, "winscp.com")

pkg.file <- paste("mlr_0.3.", rev, ".tar.gz", sep="")
pkg.path <- file.path(project.dir, "pkg", pkg.file)

cmds <- paste('/timeout=500 /command "option confirm off" "put ',pkg.file, '" "call ssh compute1 /opt/R/R-current/bin/R CMD INSTALL ', pkg.file,'" exit', sep='')

cmd  <- paste(wscp, " ", compute.login, ":", compute.pwd, "@", compute.server, compute.path, cmds, sep="") 

cat(cmd, "\n")
setwd(file.path(project.dir, "pkg"))
system(cmd)
setwd(project.dir)
