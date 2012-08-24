library(methods)
library(testthat)
library(BBmisc)
library(ParamHelpers)

  library(devtools)
  load_all("../Software/mlr/mlr/skel")
  load_all("../Software/mlr/mlrMBO/skel")
  load_all("../Software/mlr/mlrTune/skel")

configureMlr(show.learner.output=FALSE)

  
f1 = makeMBOFunction(function(x) sum(x))
ps = makeParamSet(
  makeNumericVectorParam("zz", length=2, lower=0, upper=3)
)
learner = makeLearner("regr.km", nugget.estim=TRUE)

ctrl = makeMBOControl(seq.loops=5, propose.points.method="seq.design", init.design.points=10, ensemble.select="random")

learner = makeRLEnsemble(l1,l2,l3)
mbo(f1, ps, des=NULL, learner, ctrl)

#configureMlr nicht im package
#checkArg learner fixen