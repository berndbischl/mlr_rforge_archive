
library(BatchExperiments)

prob.fun = function(obj, split, seed=1) {
  library(mlrData)
  getDataset(ds)
  rdesc = makeResampleDesc("Subsample", iters=30, split=split)
  rin = makeResampleInstance(rdesc, task=task)
  list(task=task, rin=rin)
}

for (ds in dss.classif) {
  probs[[length(probs)+1]] = makeProblem(id=ds, obj=ds, gen=prob.fun, pars=list(split=rin.splits))
}

algo.fun = function(obj, gen, learner) {
  resample(learner, gen, rdesc)  
} 

algo = makeAlgorithm(id = "resample", pars=list(learners=learners))

reg = makeRegistry()

addExperiments(reg, probs, algo)