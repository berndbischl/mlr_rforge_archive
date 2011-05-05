


source("src/_helpers/helpers.R")
source("src/runit/helpers.R")


source(file.path("src", pack, "_files.R"))
library(abind)
library(MASS)
library(e1071)
library(boot)
library(reshape)
library(mlrTune)
for (f in pack.files) {
  source(file.path("src", f))
}
source("D:\\sync\\projekte\\mlr\\src\\mlrVarsel\\smsVarselGA.R")
source("D:\\sync\\projekte\\mlr\\src\\mlrVarsel\\VarselControlMCO.R")
source("D:\\sync\\projekte\\mlr\\src\\mlrVarsel\\varselMCO.R")

require("mlbench")

parallel.setup(mode="local")
logger.setup(level="error")
errorhandler.setup()

data(Sonar, BreastCancer)


task = makeClassifTask(target="Class", data=Sonar)
w = makeLearner("classif.rpart")
res = makeResampleDesc("Holdout")

m1 = mmce
m1@aggr = list(test.mean)

#m2 = makeMeasure(id="varcosts", minimize=TRUE, classif=TRUE, regr=TRUE, allowed.pred.types="response",
#  fun=function(task, model, pred, extra.pars) {
#    10
#    #v = model@vars 
#    #feature_cost(v, dim) / feature_cost(ct["input.names"], dim)
#  }
#)
#m2@aggr = list(test.mean)


ctrl = makeVarselControlMCO(maxit=100, mu=5L, mut.prob=0.25, cross.prob=0.25, ref.point=c(1,1))
print(ctrl)
vr = varselMCO(w, task, res, measures=list(m1, m3), control=ctrl)


#m = train(w, subset(task, vars=getFeatureNames(task)[1:2]))
