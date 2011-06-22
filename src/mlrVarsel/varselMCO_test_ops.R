pack = "mlrVarsel"
source("src/_helpers/helpers.R")
source("src/runit/helpers.R")


source(file.path("src", pack, "_files.R"))
library(abind)
library(MASS)
library(e1071)
library(boot)
library(reshape)
library(emoa)
library(mlrTune)
for (f in pack.files) {
  source(file.path("src", f))
}

source("D:\\sync\\projekte\\mlr\\src\\mlrVarsel\\VarselControlMCO.R")
source("D:\\sync\\projekte\\mlr\\src\\mlrVarsel\\LearnerBag.R")
source("D:\\sync\\projekte\\mlr\\src\\mlrVarsel\\varselMCO_operators.R")
source("D:\\sync\\projekte\\mlr\\src\\mlrVarsel\\varselMCO.R")


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
  prob.mut.learner=0.5, prob.mut.bit=0.5, prob.cx=0.5, mut.hp.eta=10, mut.hp.prob=0.9)




#learner.ns = "classif.lda"
#y = sampleHyperPars(pss[["classif.lda"]])
#print(y)
#
#learner.ns = "classif.ksvm"
#y = sampleHyperPars(pss[["classif.ksvm"]])
#print(y)


#x = list(
#  learner = "classif.lda",
#  hyper.pars = numeric(0),
#  bits = c(1L,0L,0L)
#)
#
#learner.ns = "classif.lda"
#y = mutate(x, learner.ns, pss, ctrl)
#print(y)
#
#
#learner.ns = c("classif.lda", "classif.rpart", "classif.ksvm")
#y = mutate(x, learner.ns, pss, ctrl)
#print(y)




x1 = list(
  learner = "classif.lda",
  hyper.pars = numeric(0),
  bits = c(1L,0L,0L)
)

x2 = list(
  learner = "classif.rpart",
  hyper.pars = numeric(0),
  bits = c(0L,1L,1L)
)

x3 = list(
  learner = "classif.ksvm",
  hyper.pars = c(C=1, sigma=0),
  bits = c(1L,0L,0L)
)


x4 = list(
  learner = "classif.ksvm",
  hyper.pars = c(C=0, sigma=5),
  bits = c(1L,1L,0L)
)

x5 = list(
  learner = "classif.kknn",
  hyper.pars = c(k=7L),
  bits = c(1L,1L,0L)
)


print(crossover(x1, x2, pss, ctrl))
print(crossover(x1, x3, pss, ctrl))
print(crossover(x3, x4, pss, ctrl))
print(crossover(x4, x5, pss, ctrl))

print(crossover(x5, x5, pss, ctrl))

