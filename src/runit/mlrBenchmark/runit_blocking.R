test.blocking = function() {
  b = as.factor(rep(1:30, 5)) 
  ct = makeClassifTask(target=multiclass.target, data=multiclass.df, blocking=b)
  res = makeResampleDesc("CV", iters=2)
  # test blocking in bench.exp
  be = benchmark(tasks=ct, learners="classif.lda", resampling=res)
  p = be@res.results[[1]][["classif.lda"]]$pred
  res2 = be@resamplings[[1]]
  for (j in 1:res2@desc@iters) {
    test.j = p@df[p@df$iter == j, "id"]
    tab = table(b[test.j])
    checkTrue(setequal(c(0,5), unique(as.numeric(tab))))
  }
}