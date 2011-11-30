

test.parallel.warn = function() {
  res = makeResampleDesc("CV", iters=2)
  level = .mlr.conf$logger.setup$global.level
  setupLogger(level="warn")
  opwd = .mlr.conf$errorhandler.setup$on.par.without.desc
  
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
  
  setupLogger(level=level)
  errorhandler.setup(on.par.without.desc=opwd)
  parallel.setup(mode="local")
}

