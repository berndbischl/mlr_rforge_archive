library(mlr)
source("src\\base\\optimize\\myspo\\control.myspo.opt.r")
source("src\\base\\optimize\\myspo\\init.design.r")
source("src\\base\\optimize\\myspo\\seq.design.r")
source("src\\base\\optimize\\myspo\\eval.design.r")
source("src\\base\\optimize\\myspo\\choose.new.points.r")
source("src\\base\\optimize\\myspo\\choose.final.point.r")
source("src\\base\\optimize\\myspo\\spo.helpers.r")
source("src\\base\\optimize\\myspo\\meta.model.r")
source("src\\base\\optimize\\myspo\\myspo.r")

f = function(x,y,z) sum(x^2+y^2+z^2) + rnorm(1, 0, 0.001)

par.descs = list(      
  new("par.desc.num", par.name="x", lower=-1000, upper=1000),
  new("par.desc.num", par.name="y", lower=-1000, upper=1000),
  new("par.desc.num", par.name="z", lower=-1000, upper=1000)
)


ctrl = myspo.optcontrol(par.descs=par.descs, meta.learner="regr.randomForest", seq.loops=10, init.des.points=30)
z = myspo(f, control=ctrl) 
print(z[1:3])


#library(mlr)
#par.descs = list(      
#  new("par.desc.num", par.name="sigma", lower=0, upper=1000),
#  new("par.desc.num", par.name="C", lower=0, upper=1000),
#  new("par.desc.log", par.name="scaled", default=TRUE)
#)
#
#library(mlbench)
#data(Sonar)
#ct = make.task(data=Sonar, target="Class")
#wl = make.learner("classif.ksvm")
#res = make.res.desc("holdout", split=4/5)
#ctrl = myspo.control(par.descs=par.descs, init.des.points=30L, seq.des.points=5000L, meta.learner="regr.randomForest", seq.loops=200L)
#
#library(lhs)
#tr = tune.myspo(wl, ct, res, measures="mmce", aggr="mean", control = ctrl)
#print(tr[-1])
#
#res2 = make.res.instance("subsample", size=ct["size"], iters=50)
#wl.opt = make.learner("classif.randomForest", par.vals=tr$opt, id="opt")
#learners = list("classif.randomForest", wl.opt)
#be = bench.exp(tasks=ct, learners=learners, res2)
