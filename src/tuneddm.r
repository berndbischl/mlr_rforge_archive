#linear.correlation(Species~., iris)
#rank.correlation(Species~., iris)
#chi.squared(Species~., iris)
#information.gain(Species~., iris)
#gain.ratio(Species~., iris)
#symmetrical.uncertainty(Species~., iris)
#oneR(Species~., iris)
#relief(Species~., iris)
#
#linear.correlation(Class~., Vowel)
#rank.correlation(Class~., Vowel)
#chi.squared(Class~., Vowel)
#information.gain(Class~., Vowel)
#gain.ratio(Class~., Vowel)
#symmetrical.uncertainty(Class~., Vowel)
#oneR(Class~., Vowel)
#relief(Class~., Vowel)
#
#linear.correlation(medv~., BostonHousing)
#rank.correlation(medv~., BostonHousing)
#chi.squared(medv~., BostonHousing)
#information.gain(medv~., BostonHousing)
#gain.ratio(medv~., BostonHousing)
#symmetrical.uncertainty(medv~., BostonHousing)
#oneR(medv~., BostonHousing)
#relief(medv~., BostonHousing)



#source("src/files.r")
#load.all.libs()
#load.all.sources("src")
#
#logger.setup(level="info", sublevel="tune")
#parallel.setup(mode="local")
#errorhandler.setup()


#rfoob = make.measure(id="rf.ooberr", minimize=TRUE,  
#  fun=function(task, model, pred, extra.pars) {
#    x = model["learner.model"]$err.rate
#    x[nrow(x), "OOB"]
#})
#
ct = make.task(data=Sonar, target="Class")

# chain
wl = make.learner("classif.rpart", predict.type="prob")
wl = make.filter.wrapper(wl, fw.method="chi.squared", fw.threshold=3)
wl = make.probth.wrapper(wl, classes=ct["class.levels"])
res = make.res.desc("cv", iters=5)

th.ns = names(wl["par.vals", head=T])
th.st = rep(0.5, ct["class.nr"])
th.lo = rep(0.0 , ct["class.nr"])
th.up = rep(1.0, ct["class.nr"])
names(th.st) = names(th.lo) = names(th.up) = th.ns

#ctrl = cmaes.control(
#  start = c(c(fw.threshold=0.3, cp=0.1), th.st), 
#  lower = c(c(fw.threshold=-10, cp=0.0), th.lo), 
#  upper = c(c(fw.threshold=100, cp=1.0), th.up), 
#  maxit=5
#)

source("D:\\sync\\projekte\\mlr\\src\\base\\optimize\\init.des.r")
source("D:\\sync\\projekte\\mlr\\src\\base\\tune\\control.diceoptim.r")
source("D:\\sync\\projekte\\mlr\\src\\base\\tune\\tune.diceoptim.r")
source("D:\\sync\\projekte\\mlr\\src\\base\\optml\\tune.helpers.r")

vf = varfilter(ct, method="chi.squared", 1)$vals

ctrl = DiceOptim.control(
  lower = c(c(fw.threshold=min(vf), cp=0.0), th.lo), 
  upper = c(c(fw.threshold=max(vf), cp=0.3), th.up), 
  nsteps = 100,
  init.des.points=30L
)

print(ctrl)

#set.seed(1)
tr = tune(wl, ct, res, control=ctrl)
print(tr)
print(tr["path", as.data.frame=T][,1:5])

