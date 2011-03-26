if (!exists("build")) build <- TRUE
if (!exists("check")) check <- TRUE
if (!exists("binary")) binary <- FALSE
if (!exists("install")) install <- FALSE


source("src/make/make.r")
source("src/make/desc_mlr.R")
source("src/make/rev.nr.r")
source("src/make/remove.r")

make("mlr", build=build, check=check, binary=binary, install=install)
