

test.parallel.tune = function() {
  
  res = make.res.instance(make.res.desc("subsample", iters=4), task=multiclass.task)
  
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
  
  ctrl = grid.control(ranges=list(sigma=c(0.05, 0.5, 5)), trafo=function(x) x*2)
  
  parallel.setup(mode="local")
  tr11 = tune("classif.ksvm", task=multiclass.task, resampling=res, control=ctrl, path=T)
  
  parallel.setup(mode="multicore", cpus=2, level="resample")
  tr12 = tune("classif.ksvm", task=multiclass.task, resampling=res, control=ctrl, path=T)
  parallel.setup(mode="multicore", cpus=2, level="tune")
  tr13 = tune("classif.ksvm", task=multiclass.task, resampling=res, control=ctrl, path=T)
  
  parallel.setup(mode="snowfall", cpus=2, level="resample")
  tr14 = tune("classif.ksvm", task=multiclass.task, resampling=res, control=ctrl, path=T)
  parallel.setup(mode="snowfall", cpus=2, level="tune")
  tr15 = tune("classif.ksvm", task=multiclass.task, resampling=res, control=ctrl, path=T)
  
  p = tr11["path", as.data.frame=T]; p$sigma=p$sigma*2
  checkEquals(tr1["path", as.data.frame=T], p)
  p = tr12["path", as.data.frame=T]; p$sigma=p$sigma*2
  checkEquals(tr1["path", as.data.frame=T], p)
  p = tr13["path", as.data.frame=T]; p$sigma=p$sigma*2
  checkEquals(tr1["path", as.data.frame=T], p)
  p = tr14["path", as.data.frame=T]; p$sigma=p$sigma*2
  checkEquals(tr1["path", as.data.frame=T], p)
  p = tr15["path", as.data.frame=T]; p$sigma=p$sigma*2
  checkEquals(tr1["path", as.data.frame=T], p)
  
  parallel.setup(mode="local")
}