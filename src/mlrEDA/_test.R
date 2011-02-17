library(ggplot2)
library(DAAG)
source("D:\\sync\\projekte\\mlr\\src\\mlrEDA\\summarizeData.R")
source("D:\\sync\\projekte\\mlr\\src\\mlrEDA\\plotFeatureDistrib.R")
source("D:\\sync\\projekte\\mlr\\src\\mlrEDA\\plotPCA.R")
source("D:\\sync\\projekte\\mlr\\src\\mlrEDA\\writeEDAReport.R")
source("D:\\sync\\projekte\\mlr\\src\\mlrEDA\\varsel2d.R")

set.seed(1)
dd = iris
dd$foo = sample(c(T,F), 150, replace=T)
ct = makeClassifTask(data=dd, target="Species")


#report(dd, "Species")

res = makeResampleDesc("CV", 2)
ctrl = sequential.control(method="sfs", alpha=0.01)
vr = varsel2d("classif.rpart", ct, res, control=ctrl, pairs=2)

