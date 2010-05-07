source("src/make/config.r")

html.docs = file.path(project.dir, "pkg", "mlr", "chm")
www.docs = file.path(project.dir, "www", "rdocs")
if( unlink(file.path(www.docs, list.files(www.docs))) != 0) 
	stop("could not delete www doc dir!")		

docs = list.files(html.docs, all=T)
docs = docs[grep("*.html", docs)]
docs = paste(html.docs, docs, sep="/")
file.copy(from=docs, to=www.docs, recursive = TRUE) 

