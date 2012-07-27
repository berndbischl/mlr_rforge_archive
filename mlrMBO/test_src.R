library(methods)
library(testthat)

  library(devtools)
  load_all("skel")

configureMlr(show.learner.output=FALSE)

  
f1 = makeMBOFunction(function(x) 1)
ps = makeParamSet(
  makeNumericVectorParam("zz", length=2, lower=0, upper=3)
)
learner = makeLearner("regr.km", nugget.estim=TRUE)

ctrl = makeMBOControl(seq.loops=5, propose.points.method="EI", init.design.points=10)
mbo(f1, ps, des=NULL, learner, ctrl)
