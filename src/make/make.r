library(roxygen)
source("src/make/config.r")
source("src/make/desc.r")
source("src/make/rev.nr.r")
source("src/files.r")
source("src/files_rd.r")



setwd(project.dir)

src.dir  <- file.path(project.dir, "src")
pkg.dir  <- file.path(project.dir, "pkg")
clr.dir  <- file.path(pkg.dir, "clr")
r.dir    <- file.path(clr.dir, "R") 
man.dir  <- file.path(clr.dir, "man") 
html.dir <- file.path(project.dir, "html") 

cat("Building clr to :", pkg.dir, "...\n")
cat("Clean up:\n")

if( unlink(file.path(r.dir, list.files(r.dir))) != 0) 
	stop("could not delete r dir!")		
if( unlink(file.path(man.dir, list.files(man.dir))) != 0) 
	stop("could not delete man dir!")		

	#	print("Package dir deleted.")
#} else {
#  	stop("Package dir not deleted!")
#}
#unlink()
#dir.create(file.path(pkg.dir, "man"))
#dir.create(file.path(pkg.dir, "R"))


code.files <- file.path(src.dir, c(base.files, classif.files, regr.files))

cat("Build skeleton for:\n")
print(code.files)
package.skeleton("clr", code_files=code.files, path = pkg.dir, namespace=FALSE, force=TRUE)


desc.file <- file.path(clr.dir, "DESCRIPTION")
rev.nr <- get.rev.nr()
write.desc(desc.file, rev.nr)

if( unlink(file.path(man.dir, list.files(man.dir, pattern=".*-class.Rd"))) != 0) 
	stop("could not class.rd files!")		



roxygenize(package.dir=clr.dir, roxygen.dir=clr.dir, copy.package=FALSE, overwrite=TRUE)

rds <- list.files(man.dir)
for (f in rds) {
	if(!(f %in% allowed.rd.files)) {
		file.remove(file.path(man.dir, f))
	}
}

rds <- "*.Rd"
wd <- getwd()
setwd(man.dir)
cmd <- paste("R CMD Rdconv2 --type=html -o ", html.dir, " ", rds, sep="")
print(cmd)
system(cmd)

setwd(pkg.dir)
cmd <- paste("R CMD build clr")
print(cmd)
system(cmd)
cmd <- paste("R CMD check clr")
print(cmd)
s <- system(cmd, intern=TRUE)
#cmd <- paste("R CMD build --binary clr")
#print(cmd)
#system(cmd)
print(s)
err.i <- grep("Error|Fehler", s)

if (length(err.i) == 0) {
	print("make:done")
} else {
	print(err.i)
	print(s[err.i])
	print("make:failed")
}

setwd(wd)


