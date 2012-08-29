#1:
  
fun <- makeMBOFunction(function(x) sum(x^2))
par.set <-  makeParamSet(
  makeNumericVectorParam("param.vec", length=3, lower=0, upper=3)
)
learner <-  makeLearner("regr.rsm", modelfun="SO")

ctrl = makeMBOControl(seq.loops=20, propose.points.method="seq.design", init.design.points=10)
mbo(fun=fun, par.set=par.set, learner=learner, control=ctrl)

#2:
  
noise.fun <- makeMBOFunction(function(x) sum(x^2) + rnorm(1,0,2))
par.set <-  makeParamSet(
  makeNumericVectorParam("param.vec", length=3, lower=0, upper=3)
)
learner <-  makeLearner("regr.km", nugget.estim=T, covtype="powexp")

ctrl = makeMBOControl(seq.loops=5, final.evals=5, propose.points.method="EI", init.design.points=20)
mbo(fun=noise.fun, par.set=par.set, learner=learner, control=ctrl)

#3:
 
fun <- makeMBOFunction(function(x) sum(x^2))
par.set <-  makeParamSet(
  makeNumericVectorParam("param.vec", length=3, lower=-3, upper=3)
)
learner <-  makeLearner("regr.randomForest")

ctrl = makeMBOControl(seq.loops=50, propose.points.method="CMAES", init.design.points=10)
set.seed(5) 
mbo(fun=fun, par.set=par.set, learner=learner, control=ctrl)
