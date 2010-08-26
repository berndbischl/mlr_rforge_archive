

test.parallel.tune = function() {
  
  ct = make.task(data=iris, target="Species")
  multiclass.task = ct
  res = make.res.instance("subsample", iters=4, task=multiclass.task)
  
  ctrl = grid.control(ranges=list(sigma=c(0.1, 1, 10)))
  
  parallel.setup(mode="local")
  tr1 = tune("classif.ksvm", task=multiclass.task, resampling=res, control=ctrl, path=T)

  parallel.setup(mode="multicore", cpus=2, level="resample")
  tr2 = tune("classif.ksvm", task=multiclass.task, resampling=res, control=ctrl, path=T)
  parallel.setup(mode="multicore", cpus=2, level="tune")
  tr3 = tune("classif.ksvm", task=multiclass.task, resampling=res, control=ctrl, path=T)
  
  parallel.setup(mode="snowfall", cpus=2, level="resample")
  tr4 = tune("classif.ksvm", task=multiclass.task, resampling=res, control=ctrl, path=T)
  parallel.setup(mode="snowfall", cpus=2, level="tune")
  tr5 = tune("classif.ksvm", task=multiclass.task, resampling=res, control=ctrl, path=T)
  
  checkEquals(tr1["path", as.data.frame=T], tr2["path", as.data.frame=T])
  checkEquals(tr1["path", as.data.frame=T], tr3["path", as.data.frame=T])
  checkEquals(tr1["path", as.data.frame=T], tr4["path", as.data.frame=T])
  checkEquals(tr1["path", as.data.frame=T], tr5["path", as.data.frame=T])
  parallel.setup(mode="local")
}