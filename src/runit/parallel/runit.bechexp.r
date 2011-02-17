
test.parallel.benchexp = function() {
  
  tasks = c(multiclass.task, binaryclass.task)
  
  fun = function(data) data
    f1 = function(data, targetvar, args) {
    data[,2] = args$x * data[,2]
    return(list(data=data, control=list()))
  }
  f2 = function(data, targetvar, args, control) {
    data[,2] = args$x * data[,2]
    return(data)
  }  
  wl = makePreprocWrapper("classif.lda", train=f1, predict=f2, args=list(x=1, y=2))
  wl = setId(wl, "lda2")
  learners = list("classif.lda", "classif.rpart", wl)
  
  res = makeResampleDesc("CV", iters=2)

  parallel.setup(mode="local")
  be = bench.exp(tasks=tasks, learners=learners, resampling=res)
  parallel.setup(mode="multicore", cpus=2, level="bench")
  be = bench.exp(tasks=tasks, learners=learners, resampling=res)
  parallel.setup(mode="snowfall", cpus=2, level="bench")
  be = bench.exp(tasks=tasks, learners=learners, resampling=res)
  parallel.setup(mode="local")
}