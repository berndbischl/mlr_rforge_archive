

test.parallel.warn = function() {
  res = make.res.instance("cv", iters=2, task=binaryclass.task)

  #todo: for some reasons i get strange erorrs here 
  
#  parallel.setup(mode="multicore", level="resample", cpus=2)
#  checkWarning(
#      resample.fit("classif.rpart", binaryclass.task, res, par.vals=list(foo=1)),
#      "Setting par foo without"  
#  )
  
  parallel.setup(mode="snowfall", level="resample", cpus=2)
  checkWarning(
      resample.fit("classif.rpart", binaryclass.task, res, par.vals=list(foo=1)),
      "Setting par foo without"  
  )
  
  errorhandler.setup(on.par.without.desc="quiet")
  
#  parallel.setup(mode="multicore", level="resample", cpus=2)
#  checkWarning(
#      resample.fit("classif.rpart", binaryclass.task, res, par.vals=list(foo=1)),
#      "Setting par foo without"  
#  )
  
  parallel.setup(mode="snowfall", level="resample", cpus=2)
  checkWarning(
      resample.fit("classif.rpart", binaryclass.task, res, par.vals=list(foo=1)),
      "Setting par foo without"  
  )

  errorhandler.setup()
}

