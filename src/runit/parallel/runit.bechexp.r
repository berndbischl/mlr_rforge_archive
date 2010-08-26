
test.parallel.benchexp = function() {
  
  tasks = c(multiclass.task, binaryclass.task)
  learners = c("classif.lda", "classif.rpart")
  res = make.res.desc("cv", iters=2)

  parallel.setup(mode="local")
  be = bench.exp(tasks=tasks, learners=learners, resampling=res)
  parallel.setup(mode="multicore", cpus=2, level="bench")
  be = bench.exp(tasks=tasks, learners=learners, resampling=res)
  parallel.setup(mode="snowfall", cpus=2, level="bench")
  be = bench.exp(tasks=tasks, learners=learners, resampling=res)
}