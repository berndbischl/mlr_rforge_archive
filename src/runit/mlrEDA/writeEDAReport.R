require(brew)
d.set <- iris
target <- "Species"
name <- "iris"
writeEDAReport(tempdir(),d.set,target,name)
checkTrue("iris.html" %in% list.files(tempdir()))