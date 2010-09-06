#todo: learn 2nd meta model for constraint violation, so we only generate reasonable points in seqdes?
# we need confidence ingtervals in prediction! then we can use EI 
tune.myspo = function(learner, task, resampling, measures, aggr, control) {
  pds = control["par.descs"]
  control@constr.learner = make.learner("classif.randomForest")
  ml = control["meta.learner"]
  cl = control["constr.learner"]  
  
  curdes = init.design(pds, control@init.des.points)
  cury = eval.des.with.learner(curdes, learner, task, resampling, measures, aggr, control)
  print(cbind(curdes, cury))
  tmm = train.meta.model(ml, cl, curdes, cury)
  
  loop = 1  
  while(loop <= control["seq.loops"]) {
    print(loop)
    seqdes = seq.design(pds, control@seq.des.points, tmm$constr.model)
    y = eval.des.with.meta.model(seqdes, tmm$meta.model)
    j = choose.new.points(1, seqdes, y)
    newdes = seqdes[j,]
    newy = eval.des.with.learner(newdes, learner, task, resampling, measures, aggr, control)
    print(cbind(newdes, newy))
    curdes = rbind(curdes, newdes)
    cury = c(cury, newy)
    tmm = train.meta.model(ml, cl, curdes, cury)
    loop = loop + 1    
  }
  finaldes = cbind(curdes, y=cury)
  fp = choose.final.point(tmm$meta.model, tmm$constrmodel, learner, task, resampling, measures, aggr, control)
  list(path=finaldes, opt=fp$x, y.meta=fp$y.meta, y.real=fp$y.real, y.diff=fp$y.diff)  
}

library(mlr)
par.descs = list(      
  new("par.desc.num", par.name="sigma", lower=0, upper=1000),
  new("par.desc.num", par.name="C", lower=0, upper=1000),
  new("par.desc.log", par.name="scaled", default=TRUE)
)

library(mlbench)
data(Sonar)
ct = make.task(data=Sonar, target="Class")
wl = make.learner("classif.ksvm")
res = make.res.desc("holdout", split=4/5)
ctrl = myspo.control(par.descs=par.descs, init.des.points=30L, seq.des.points=5000L, meta.learner="regr.randomForest", seq.loops=200L)

library(lhs)
source("D:\\sync\\projekte\\mlr\\src\\base\\myspo\\init.design.r")
source("D:\\sync\\projekte\\mlr\\src\\base\\myspo\\eval.design.r")
source("D:\\sync\\projekte\\mlr\\src\\base\\myspo\\choose.final.point.r")
source("D:\\sync\\projekte\\mlr\\src\\base\\myspo\\spo.helpers.r")
source("D:\\sync\\projekte\\mlr\\src\\base\\myspo\\meta.model.r")
tr = tune.myspo(wl, ct, res, measures="mmce", aggr="mean", control = ctrl)
print(tr[-1])

res2 = make.res.instance("subsample", size=ct["size"], iters=50)
wl.opt = make.learner("classif.randomForest", par.vals=tr$opt, id="opt")
learners = list("classif.randomForest", wl.opt)
be = bench.exp(tasks=ct, learners=learners, res2)


