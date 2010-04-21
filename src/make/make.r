library(roxygen)
source("src/make/config.r")
source("src/make/desc.r")
source("src/make/rev.nr.r")
source("src/make/remove.r")
source("src/files.r")
source("src/files_rd.r")




make <- function(only.allowed.rds=TRUE, build=TRUE, check=TRUE, binary=FALSE, install=FALSE) {
	src.dir  <<- file.path(project.dir, "src")
	pkg.dir  <<- file.path(project.dir, "pkg")
	
	build.dir  <<- file.path(pkg.dir, "mlr")
	rox.dir  <<- file.path(pkg.dir, "mlr.roxygen")
	r.build.dir    <<- file.path(build.dir, "R") 
	r.rox.dir    <<- file.path(rox.dir, "R") 
	man.build.dir  <<- file.path(build.dir, "man") 
	man.rox.dir  <<- file.path(rox.dir, "man") 
	data.build.dir  <<- file.path(build.dir, "data") 
	#html.dir <- file.path(project.dir, "html") 
	
	cat("Building mlr to :", pkg.dir, "...\n")
	cat("Clean up:\n")
	
	if( unlink(file.path(r.build.dir, list.files(r.build.dir))) != 0) 
		stop("could not delete r dir!")		
	if( unlink(file.path(man.build.dir, list.files(man.build.dir))) != 0) 
		stop("could not delete man dir!")		
	
	code.files <- file.path(src.dir, c(base.files, classif.files, regr.files))
	
	package.skeleton("mlr", code_files=code.files, path = pkg.dir, namespace=FALSE, force=TRUE)
	
	desc.file <- file.path(build.dir, "DESCRIPTION")
	rev.nr <- get.rev.nr()
	write.desc(desc.file, rev.nr)

	roxygenize(package.dir=build.dir, use.Rd2=TRUE, unlink.target=TRUE)


	file.copy(from=man.rox.dir, to=build.dir, recursive = TRUE) 
	file.copy(from=file.path(rox.dir, "NAMESPACE"), to=build.dir, overwrite = TRUE) 
	file.copy(from=file.path(rox.dir, "DESCRIPTION"), to=build.dir, overwrite = TRUE) 
	
	
	
	if (only.allowed.rds) {
		rds <- list.files(man.build.dir, all=T)
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
	
#rds <- "*.Rd"
#wd <- getwd()
#setwd(man.dir)
	##cmd <- paste("R CMD Rdconv2 --type=html -o ", html.dir, " ", rds, sep="")
	##print(cmd)
	##system(cmd)
#
	if (build || install) {
		setwd(pkg.dir)
		cmd <- paste("R CMD build mlr")
		print(cmd)
		system(cmd)
	}
	if (binary) {
		setwd(pkg.dir)
		cmd <- paste("R CMD build --binary mlr")
		print(cmd)
		system(cmd)
	}
	if (check) {
		setwd(pkg.dir)
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
	
	
	if (install) {
		fs = sort(list.files(pkg.dir, pattern="mlr.*tar.gz"))
		f = fs[length(fs)]
		f = file.path(pkg.dir, f)
		paste("Installing", f, "\n")
		cmd <- paste("R CMD INSTALL", f)
		print(cmd)
		system(cmd)
	}
	setwd(project.dir)
	
}

make(build=F, check=F, binary=F, install=T)



