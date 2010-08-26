

test.parallel.resample = function() {
  wl = make.learner("classif.lda", predict.type="prob")
  res = make.res.instance("subsample", iters=4, task=binaryclass.task)

  parallel.setup(mode="local")
  p1 = resample.fit(wl, binaryclass.task, res)
  parallel.setup(mode="multicore", level="resample", cpus=2)
  p2 = resample.fit(wl, binaryclass.task, res)
  parallel.setup(mode="snowfall", level="resample", cpus=2)
  p3 = resample.fit(wl, binaryclass.task, res)
  
  checkEquals(p1["response"], p2["response"])
  checkEquals(p1["response"], p3["response"])
  parallel.setup(mode="local")
}

