
context("resample")

test_that("resample", {
  rin1 = makeResampleInstance(makeResampleDesc("BS", iters=4), task=multiclass.task)  
  rin2 = makeResampleInstance(makeResampleDesc("CV", iters=7), task=multiclass.task)  
  rin3 = makeResampleInstance(makeResampleDesc("Subsample", iters=2), task=multiclass.task)  
  
  lrn = makeLearner("classif.lda")
  p1 = resample(lrn, multiclass.task, rin1)$pred       
  p2 = resample(lrn, multiclass.task, rin2)$pred       
  p3 = resample(lrn, multiclass.task, rin3)$pred       
  
  inds = Reduce(c, rin1$test.inds)
  y = getTargets(multiclass.task)[inds]
  expect_equal(p1$df$id, inds)
  expect_equal(p1$df$truth, y)
  inds = Reduce(c, rin2$test.inds)
  y = getTargets(multiclass.task)[inds]
  expect_equal(p2$df$id, inds)
  expect_equal(p2$df$truth, y)
  inds = Reduce(c, rin3$test.inds)
  y = getTargets(multiclass.task)[inds]
  expect_equal(p3$df$id, inds)
  expect_equal(p3$df$truth, y)

  cv.i = makeResampleInstance(makeResampleDesc("CV", iters=3), binaryclass.task)
  
  lrn1 = makeLearner("classif.lda")
  lrn2 = makeLearner("classif.lda", predict.type="prob")
  rf1 = resample(lrn1, binaryclass.task, cv.i)$pred
  rf2 = resample(lrn2, binaryclass.task, cv.i)$pred
  rf3 = resample(lrn2, binaryclass.task, cv.i)$pred
  rf3 = setThreshold(rf3, 0)
  rf4 = resample(lrn2, binaryclass.task, cv.i)$pred
  rf4 = setThreshold(rf4, 1)
  
  expect_equal(rf1$df$response, rf2$df$response)
  f1 = factor(rep(binaryclass.task$desc$positive, cv.i$size), levels=binaryclass.task$desc$class.levels)
  expect_equal(rf3$df$response, f1)
  f2 = factor(rep(binaryclass.task$desc$negative, cv.i$size), levels=binaryclass.task$desc$class.levels)
  expect_equal(rf4$df$response, f2)
  
  ct = makeClassifTask(data=iris[,c("Species", "Petal.Width")], target="Species")
  fit = resample(lrn1, ct, makeResampleDesc("CV", iters=2))
  
})
