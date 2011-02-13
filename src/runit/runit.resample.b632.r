test.b632 <- function() {
  res = makeResampleDesc("bs", iters=2)
  m = setAggr(mmce, b632)
  r = resample("classif.rpart", task=binaryclass.task, resampling=res)
  m1 = r$measures.train
  m2 = r$measures.test
  p = as.data.frame(r$pred)
  ls11 = p[p$set == "train" & p$iter==1, c("truth", "response")]
  ls12 = p[p$set == "test" & p$iter==1, c("truth", "response")]
  ls1 = 0.368*mean(ls11[,1] != ls11[,2]) + 0.632*mean(ls12[,1] != ls12[,2])
  ls21 = p[p$set == "train" & p$iter==2, c("truth", "response")]
  ls22 = p[p$set == "test" & p$iter==2, c("truth", "response")]
  ls2 = 0.368*mean(ls21[,1] != ls21[,2]) + 0.632*mean(ls22[,1] != ls22[,2])
  # ag = perf1$aggr.group
  # checkEquals(ls1, ag[1, "mmce"])
  # checkEquals(ls2, ag[2, "mmce"])
  # checkEquals(mean(c(ls1, ls2)), perf1$aggr[1, "mmce"])
  #  # check that combine works at least
  #  p2 = as(p, "grouped.Prediction")
  #  checkTrue(setequal(colnames(p2@df), c("truth", "response", "id", "group")))
  # perf2 = performance(p, measures=c("mmce"), aggr="combine")
}