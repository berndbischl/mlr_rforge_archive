library(roxygen)
source("src/make/config.r")
source("src/make/desc.r")
source("src/make/rev.nr.r")
source("src/files.r")
source("src/files_rd.r")




make <- function(build=TRUE, check=TRUE, binary=FALSE) {
	src.dir  <- file.path(project.dir, "src")
	pkg.dir  <- file.path(project.dir, "pkg")
	skel.dir  <- file.path(pkg.dir, "mlr.skel")
	build.dir  <- file.path(pkg.dir, "mlr")
	r.skel.dir    <- file.path(skel.dir, "R") 
	r.build.dir    <- file.path(build.dir, "R") 
	man.skel.dir  <- file.path(skel.dir, "man") 
	man.build.dir  <- file.path(build.dir, "man") 
	#html.dir <- file.path(project.dir, "html") 
	
	cat("Building mlr to :", pkg.dir, "...\n")
	cat("Clean up:\n")
	
	if( unlink(file.path(r.skel.dir, list.files(r.skel.dir))) != 0) 
		stop("could not delete r dir!")		
	if( unlink(file.path(man.skel.dir, list.files(man.skel.dir))) != 0) 
		stop("could not delete man dir!")		
	if( unlink(file.path(man.build.dir, list.files(man.build.dir))) != 0) 
		stop("could not delete man dir!")		
	
	code.files <- file.path(src.dir, c(base.files, classif.files, regr.files))
	
	cat("Build skeleton for:\n")
	print(code.files)
	package.skeleton("mlr.skel", code_files=code.files, path = pkg.dir, namespace=FALSE, force=TRUE)
	
	desc.file <- file.path(skel.dir, "DESCRIPTION")
	rev.nr <- get.rev.nr()
	write.desc(desc.file, rev.nr)
	
	roxygenize(package.dir=skel.dir, roxygen.dir=build.dir, use.Rd2=TRUE, copy.package=FALSE)
	
	rds <- list.files(man.build.dir)
	for (f in rds) {
		if(!(f %in% allowed.rd.files)) {
			file.remove(file.path(man.build.dir, f))
		}
	}
	
	file.copy(from=code.files, to=r.build.dir, overwrite = TRUE) 
	
#rds <- "*.Rd"
#wd <- getwd()
#setwd(man.dir)
	##cmd <- paste("R CMD Rdconv2 --type=html -o ", html.dir, " ", rds, sep="")
	##print(cmd)
	##system(cmd)
#
	if (build) {
		setwd(pkg.dir)
		cmd <- paste("R CMD build mlr")
		print(cmd)
		system(cmd)
	}
	if (binary) {
		cmd <- paste("R CMD build --binary mlr")
		print(cmd)
		system(cmd)
	}
	if (check) {
		cmd <- paste("R CMD check mlr")
		print(cmd)
		s <- system(cmd, intern=TRUE)
		print(s)
		err.i <- grep("Error|Fehler|ERROR", s)
		if (length(err.i) == 0) {
			print("make:done")
		} else {
			print(err.i)
			print(s[err.i])
			print("make:failed")
		}
	}
	setwd(project.dir)
}

make(build=T, check=T)


