library("methods")
library("roxygen")
library("tools")

if (file.exists("src/make/config.r")) {
  source("src/make/config.r")
} else {
  project.dir <- getwd()
}

run_command <- function(cmd, ..., intern=FALSE, msg=NULL) {
  fullcmd <- sprintf(cmd, ...)
  message("--------------------------------------------------------------------------------")
  if (!is.null(msg))
    message(msg)  
  message(sprintf("Running '%s'", fullcmd))
  t <- system.time(out <- system(fullcmd, intern=intern))
  message(sprintf("'%s' finished in %ss", fullcmd, t[3]))
  out
}

show_warnings <- function() {
  w <- warnings()
  if (!is.null(w)) {
    message("Warnings:")
    cat(w, sep="\n")
  }  
}

 
make <- function(pack.name, only.allowed.rds=TRUE, build=TRUE, check=TRUE, binary=FALSE, install=FALSE) {
	src.dir  <<- file.path(project.dir, "src")
	pkg.dir  <<- file.path(project.dir, "pkg")
	
	build.dir  <<- file.path(pkg.dir, pack.name)
	rox.dir  <<- file.path(pkg.dir, paste(pack.name, "roxygen", sep="."))
	r.build.dir    <<- file.path(build.dir, "R") 
	r.rox.dir    <<- file.path(rox.dir, "R") 
	man.build.dir  <<- file.path(build.dir, "man") 
	man.rox.dir  <<- file.path(rox.dir, "man") 
	data.build.dir  <<- file.path(build.dir, "data") 
	#html.dir <- file.path(project.dir, "html") 
	
	message("Building package to '", pkg.dir, "' ...")
	message("Cleaning up ...")
	
	if(unlink(file.path(r.build.dir, list.files(r.build.dir))) != 0) 
		stop(sprintf("Could not delete directory '%s'.", r.build.dir))		
	if(unlink(file.path(man.build.dir, list.files(man.build.dir))) != 0)
    stop(sprintf("Could not delete directory '%s'.", man.build.dir))		
	
	code.files <- file.path(src.dir, pack.files)

  message("--------------------------------------------------------------------------------")
  message(sprintf("Creating base package under '%s'.", pkg.dir))
  message("Removing stale R and Rd files ...")
  file.remove(list.files(file.path(build.dir, "R"), full.names=TRUE))
  file.remove(list.files(file.path(build.dir, "man"), full.names=TRUE))
  
  message("Creating pkg directory structure ...")
  dir.create(build.dir)
  dir.create(file.path(build.dir, "man"))
  dir.create(file.path(build.dir, "R"))

  message("Copying R files ...")
  file.copy(code.files, file.path(build.dir, "R"))

  message("Creating DESCRIPTION file ...")
	desc.file <- file.path(build.dir, "DESCRIPTION")
	rev.nr <- get.rev.nr()
	writeDesc(desc.file, rev.nr)

  message("--------------------------------------------------------------------------------")
  message("Generating documentation")
  message("Running Roxygen ...")
	ro <- capture.output(roxygenize(package.dir=build.dir, use.Rd2=TRUE, unlink.target=TRUE))
  idx <- c(grep("omitted", ro), grep("Processing", ro))
  cat(ro[-idx], sep="\n")

  message("Merging Roxygen output with base package ...")
	file.copy(from=man.rox.dir, to=build.dir, recursive = TRUE)
	file.copy(from=file.path(rox.dir, "NAMESPACE"), to=build.dir, overwrite = TRUE) 
	file.copy(from=file.path(rox.dir, "DESCRIPTION"), to=build.dir, overwrite = TRUE)
  
  message("Copying ROCR documentation into base package ...")
	man.rocr.dir = file.path(src.dir, "base", "rocr", "man")
	file.copy(from=file.path(man.rocr.dir, list.files(man.rocr.dir)), to=man.build.dir)
		
	if (only.allowed.rds) {
		rds <- list.files(man.build.dir, all=TRUE)
		for (f in rds) {
			if (f != "." && f != ".." && f != ".svn") {
				if(!(f %in% allowed.rd.files)) {
					file.remove(file.path(man.build.dir, f))
				} else {
					remove.exp(file.path(man.build.dir, f))
				}
			}
		}
	}	
	
	unlink(data.build.dir, recursive=TRUE)
  unlink(rox.dir, recursive=TRUE)
  
	if (build || install) {
		setwd(pkg.dir)
    run_command(paste("R CMD build", pack.name), msg="Building package")
	}
	if (binary) {
		setwd(pkg.dir)
    run_command(paste("R CMD build --binary", pack.name), msg="Bulding binary package")
	}
	if (check) {
		setwd(pkg.dir)
    s <- run_command(paste("R CMD check", pack.name), intern=TRUE)
    cat(s, sep="\n")
		err.i <- grep("Error|Fehler|ERROR", s)
		if (length(err.i) > 0) {
      message("--------------------------------------------------------------------------------")
			message("Errors:")
			cat(s[err.i], sep="\n")
		} else {
      ## Remove crufty directory...
      unlink(file.path(pkg.dir, paste(pack.name, "Rcheck", sep=".")), recursive=TRUE)
    }
	}
		
	if (install) {
		fs = sort(list.files(pkg.dir, pattern=paste(pack.name, "*tar.gz", sep=".")))
		f = fs[length(fs)]
		f = file.path(pkg.dir, f)
    run_command("R CMD INSTALL --html %s", f, msg="Installing package")
	}
  message("--------------------------------------------------------------------------------")
	setwd(project.dir)
}


