if (!exists("build")) build <- TRUE
if (!exists("check")) check <- TRUE
if (!exists("binary")) binary <- FALSE
if (!exists("install")) install <- FALSE

#source("src/make/rev.nr.r")
#source("src/make/remove.r")

source("src/make/desc_mlr.R")
source("src/mlr/_files.r")
source("src/mlr/_files_rd.r")

make(build=build, check=check, binary=binary, install=install)
