source("src/make/config.r")


make(build=FALSE, check=FALSE, binary=FALSE, install=TRUE)

html.docs = file.path(mlr.install.dir, "html")
www.docs = file.path(project.dir, "www", "rdocs")
if( unlink(file.path(www.docs, list.files(www.docs))) != 0) 
	stop("could not delete www doc dir!")		

docs = list.files(html.docs, all=TRUE)
docs = docs[grep("*.html", docs)]
docs = paste(html.docs, docs, sep="/")
file.copy(from=docs, to=www.docs, recursive = TRUE) 

