library(methods)
library(testthat)
library(devtools)
library(cmaes)
library(soobench)
load_all("skel", reset=TRUE)
source('~/cos/mlr/mlrMBO/skel/R/exampleRun.R')
source('~/cos/mlr/mlrMBO/skel/R/exampleRun_plot_2d_numeric.R')
configureMlr(show.learner.output=FALSE, on.learner.error="stop")

set.seed(1)

 
#objfun = function(x) {
#  sum(x$x^2)
#}
#par.set = makeNumericParamSet(id="x", len=2, lower=-2, upper=2)

# objfun = generate_branin_function()
# par.set = makeNumericParamSet(id="x", len=2, 
#   lower=lower_bounds(objfun), upper=upper_bounds(objfun))
# objfun = makeMBOFunction(objfun)

 
objfun = function(x) {
  x = x$x
  (6*x - 2)^2 * sin(12 * x - 4)
}
par.set = makeNumericParamSet(id="x", len=1, lower=0, upper=1)

#objfun = function(x) {
#  x = as.integer(x$x)
#  x
#}
#par.set = makeParamSet(
#  makeDiscreteParam(id="x", values=1:7)
#)

learner = makeLearner("regr.km", predict.type="se")
ctrl = makeMBOControl(noisy=FALSE, init.design.points=10, iters=5, infill.crit="ei", 
  #infill.opt.restarts=5, infill.opt="cmaes", infill.opt.cmaes.control=list(maxit=50), 
  infill.opt="random", infill.opt.random.points=500 
)

z = exampleRun(objfun, par.set, learner, control=ctrl, n=50)
print(z)
plot(z)


#braninfun = generate_branin_function()
#z = exampleRun(makeMBOFunction(braninfun), lower=lower_bounds(braninfun), upper=upper_bounds(braninfun), dimension = 2, control = control)
#plot.ExampleRun(z)