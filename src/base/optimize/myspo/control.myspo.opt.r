myspo.optcontrol = function(minimize=TRUE, par.descs, scale=identity, 
  meta.learner="regr.randomForest", constr.learner="classif.randomForest", 
  init.des.points=50L, seq.des.points=10000L, seq.loops=500, ...) {
  list(minimize=minimize, par.descs=par.descs, scale=identity, 
    meta.learner=meta.learner, constr.learner=constr.learner, 
    init.des.points=init.des.points, seq.des.points=seq.des.points, seq.loops=seq.loops,
    y.name = "ymyspo")
}

