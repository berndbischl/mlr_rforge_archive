pack = "mlrVarsel"
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
source("D:\\sync\\projekte\\mlr\\src\\mlrVarsel\\LearnerBag.R")
source("D:\\sync\\projekte\\mlr\\src\\mlrVarsel\\varselMCO.R")
source("D:\\sync\\projekte\\mlr\\src\\mlrVarsel\\makeVarCostMeasure.R")
source("D:\\sync\\projekte\\mlr\\src\\mlrVarsel\\plotEAF.R")

require("mlbench")

parallel.setup(mode="local")
logger.setup(level="error")
errorhandler.setup()

data(Sonar, BreastCancer)


task = makeClassifTask(target="Class", data=Sonar)
learners = list(
 makeLearner("classif.rpart"),
 makeLearner("classif.lda"),
 makeLearner("classif.naiveBayes"),
 makeLearner("classif.ksvm"),
  makeLearner("classif.kknn")
)

res = makeResampleDesc("Holdout")

m1 = mmce
m1@aggr = list(test.mean)

m2 = makeVarCostMeasure(fun=function(v) {
    length(v)
  }
)
m2@aggr = list(test.mean)

m3 = nvars
m3@aggr = list(test.mean)

ps.ksvm = makeParameterSet(
  makeNumericParameter("sigma", lower=-10, upper=10, trafo=function(x) 2^x),
  makeNumericParameter("C", lower=-10, upper=10, trafo=function(x) 2^x)
)

ps.kknn = makeParameterSet(
  makeIntegerParameter("k", lower=1L, upper=10L)
)


pss = list(
  classif.lda = makeParameterSet(),
  classif.naiveBayes = makeParameterSet(),
  classif.rpart = makeParameterSet(),
  classif.ksvm = ps.ksvm,
  classif.kknn = ps.kknn
)
ctrl = makeVarselControlMCO(maxit=500L, mu=50L, prob.init=0.5, 
  prob.mut.learner=0.01, prob.mut.bit=0.05, prob.cx=0.5, mut.hp.eta=10, mut.hp.prob=0.2)
print(ctrl)
mmv = c(mmce.test.mean=1, varcosts.test.mean=m2@extra.args[[1]](getFeatureNames(task)))
ops = varselMCO(learners, task, res, measures=list(m1, m2), control=ctrl, measure.max.vals=mmv, multi.starts=5, par.sets=pss)
#dd = as.data.frame(ops[[1]])
#print(dd$mmce.test.mean)
#plot(dd$mmce.test.mean, dd$varcosts.test.mean)
dfs = lapply(ops, as.data.frame)
df = do.call(rbind, dfs)
y.names = c("mmce.test.mean", "varcosts.test.mean")
print(pareto_plot(dfs, y.names, y.names[1], y.names[2], shape="learner", alpha="dob", color="is_run_dominated") + geom_jitter())
#plotEAF(ops, c("mmce.test.mean", "varcosts.test.mean"))

#m = train(w, subset(task, vars=getFeatureNames(task)[1:2]))
