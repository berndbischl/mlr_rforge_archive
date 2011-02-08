if (!exists("build")) build <- TRUE
if (!exists("check")) check <- TRUE
if (!exists("binary")) binary <- FALSE
if (!exists("install")) install <- FALSE


source("src/make/make.r")
source("src/make/desc_mlrEDA.R")
source("src/make/rev.nr.r")
source("src/make/remove.r")
source("src/mlrEDA/_files.R")
source("src/mlrEDA/_files_rd.R")

make("mlrEDA", build=build, check=check, binary=binary, install=install)
