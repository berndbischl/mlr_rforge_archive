source("src/make/config.r")

f = "c:/test.pdf"
wscp <- file.path(winscp.path, "winscp.com")

html.docs = file.path(project.dir, "pkg", "mlr", "chm")
setwd(html.docs)

cmd  <- paste(wscp, " ", docs.login, ":", docs.pwd, "@", docs.server, docs.path, ' /command "rm *" "put *.html" exit', sep="") 

cat(cmd, "\n")
system(cmd)
setwd(project.dir)
