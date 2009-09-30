rox.file <- function(f) {
	src.dir  <<- file.path(project.dir, "src")
	pkg.dir  <<- file.path(project.dir, "pkg")
	man.dir  <<- file.path(pkg.dir, "man.test") 
	
	if( unlink(file.path(man.dir, list.files(man.dir))) != 0) 
		stop("could not clean man test dir!")		
	rd <- make.Rd2.roclet(man.dir)
	rd$parse(file.path(src.dir, f))
	fs <- list.files(man.dir)
	sapply(fs, function(x) {
				out <- file.path(man.dir, paste(x,".html", sep=""))
				cmd <- paste("R CMD Rdconv --type=html -o ", out, " ", file.path(man.dir,x), sep="")
				print(cmd)
				system(cmd)
			})
	
}


rox.file("base/benchmark.r")