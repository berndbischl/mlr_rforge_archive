#1:
  
mbo.fun <- makeMBOFunction(function(x) sum(x^2))
par.set <-  makeParamSet(
  makeNumericVectorParam("param.vec", len=3, lower=0, upper=3)
)
learner <-  makeLearner("regr.rsm", modelfun="SO")

ctrl = makeMBOControl(seq.loops=10, propose.points.method="seq.design", init.design.points=20)
res=mbo(fun=mbo.fun, par.set=par.set, learner=learner, control=ctrl)
(opt.path <- data.frame(res$path) )
#2:
  
noise.fun <- makeMBOFunction(function(x) sum(x^2) + rnorm(1,0,2))
par.set <-  makeParamSet(
  makeNumericVectorParam("param.vec", len=3, lower=0, upper=3)
)
learner <-  makeLearner("regr.km", nugget.estim=T, covtype="powexp")

ctrl = makeMBOControl(seq.loops=5, final.evals=5, propose.points.method="EI", init.design.points=20, save.model.at=1:5)
res=mbo(fun=noise.fun, par.set=par.set, learner=learner, control=ctrl)

lapply(res$models, function(d) d$learner.model)

#3:
require("soobench") 
fun <- branin_function()
mbo.fun <- makeMBOFunction(fun)
par.set <-  makeParamSet(
  makeNumericVectorParam("param.vec", len=2, lower=lower_bounds(fun), upper=upper_bounds(fun))
)
learner <-  makeLearner("regr.randomForest")

ctrl = makeMBOControl(seq.loops=25, propose.points.method="CMAES", init.design.points=10)
#set.seed(5) 
mbo(fun=mbo.fun, par.set=par.set, learner=learner, control=ctrl)
