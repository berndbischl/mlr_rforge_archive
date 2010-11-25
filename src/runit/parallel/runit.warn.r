

test.parallel.warn = function() {
  res = make.res.instance("cv", iters=2, task=binaryclass.task)
  level = .mlr.local$logger.setup$global.level
  logger.setup(level="warn")
  opwd = .mlr.local$errorhandler.setup$on.par.without.desc
  
  #todo: for some reasons i get strange erorrs here 
#  parallel.setup(mode="multicore", level="resample", cpus=2)
#  checkWarning(
#      resample("classif.rpart", binaryclass.task, res, par.vals=list(foo=1)),
#      "Setting par foo without"  
#  )
  
  errorhandler.setup(on.par.without.desc="warn")
  parallel.setup(mode="snowfall", level="resample", cpus=2)
  checkWarning(
      resample("classif.ksvm", binaryclass.task, res, par.vals=list(foo=1)),
      "Setting par foo without"  
  )
  
  errorhandler.setup(on.par.without.desc="quiet")
  
#  parallel.setup(mode="multicore", level="resample", cpus=2)
#  checkWarning(
#      resample("classif.rpart", binaryclass.task, res, par.vals=list(foo=1)),
#      "Setting par foo without"  
#  )
  
  parallel.setup(mode="snowfall", level="resample", cpus=2)
  checkWarning(
    resample("classif.ksvm", binaryclass.task, res, par.vals=list(foo=1)),
     F
  )
  
  logger.setup(level=level)
  errorhandler.setup(on.par.without.desc=opwd)
  parallel.setup(mode="local")
}

