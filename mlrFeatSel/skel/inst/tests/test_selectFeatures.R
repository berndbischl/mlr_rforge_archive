context("selectFeatures")

test_that("selectFeatures", {
  inner = makeResampleDesc("CV", iter=2)
  
  # check all methods
  ctrl = makeFeatSelControlExhaustive(max.vars=2)
  fr = selectFeatures("classif.lda", task=multiclass.task, resampling=inner, control=ctrl)
  expect_equal(getOptPathLength(fr$path), 11) 
  expect_equal(nrow(as.data.frame(fr$path)), 11) 
  expect_equal(ncol(as.data.frame(fr$path)), 7) 
  # test printing
  print(fr)
  
  # check maxit
  ctrl = makeFeatSelControlRandom(maxit=4, path=TRUE)
  fr = selectFeatures("classif.lda", task=multiclass.task, resampling=inner, control=ctrl)
  expect_equal(getOptPathLength(fr$path), 4) 
  
  ctrl = makeFeatSelControlSequential(method="sfs", alpha=0.01, path=TRUE)
  fr = selectFeatures("classif.lda", task=multiclass.task, resampling=inner, control=ctrl)
  checkTrue(getOptPathLength(fr$path) > 1) 
  
  ctrl = makeFeatSelControlSequential(method="sbs", beta=0.01, path=TRUE)
  fr = selectFeatures("classif.lda", task=multiclass.task, resampling=inner, control=ctrl)
  checkTrue(getOptPathLength(fr$path) > 1) 
  
  ctrl = makeFeatSelControlSequential(method="sffs", alpha=0.01, path=TRUE)
  fr = selectFeatures("classif.lda", task=multiclass.task, resampling=inner, control=ctrl)
  # we must at least try to select a 2nd feature
  checkTrue(getOptPathLength(fr$path) >= 1 + 4 + 1 + 3) 
  
  ctrl = makeFeatSelControlSequential(method="sfbs", beta=0.01, path=TRUE)
  fr = selectFeatures("classif.lda", task=multiclass.task, resampling=inner, control=ctrl)
  checkTrue(getOptPathLength(fr$path) > 1) 
  
  
  
  # check max.vars
  ctrl = makeFeatSelControlSequential(alpha=0, max.vars=1, method="sfs", path=TRUE)
  fr = selectFeatures("classif.lda", task=binaryclass.task, resampling=inner, control=ctrl)
  expect_equal(length(fr$x), 1) 
  
  ctrl = makeFeatSelControlSequential(beta=1, max.vars=58, method="sbs", path=TRUE)
  fr = selectFeatures("classif.lda", task=binaryclass.task, resampling=inner, control=ctrl)
  expect_equal(length(fr$x), 58) 
  
  # check empty model
  ctrl = makeFeatSelControlSequential(method="sfs", alpha=10)
  fr = selectFeatures("classif.lda", task=multiclass.task, resampling=inner, control=ctrl)
  expect_equal(fr$x, character(0)) 
  
  # check bits
  bns = c("b1", "b2")
  btf = function(b, task) {
    fns = getFeatureNames(task)
    Reduce(c, list(fns[1:2], fns[3:4])[as.logical(b)], init=character(0))
  } 
  
  ctrl = makeFeatSelControlRandom(maxit=3)
  fr = selectFeatures("classif.lda", task=multiclass.task, resampling=inner, bit.names=bns, bits.to.features=btf, control=ctrl)
  df = as.data.frame(fr$path) 
  expect_equal(colnames(df), c("b1", "b2", "mmce.test.mean", "dob", "eol"))
  expect_equal(nrow(df), 3)
  
  ctrl = makeFeatSelControlExhaustive()
  fr = selectFeatures("classif.lda", task=multiclass.task, resampling=inner, bit.names=bns, bits.to.features=btf, control=ctrl)
  df = as.data.frame(fr$path) 
  expect_equal(colnames(df), c("b1", "b2", "mmce.test.mean", "dob", "eol"))
  expect_equal(nrow(df), 4)
  
  ctrl = makeFeatSelControlSequential(method="sfs", alpha=0)
  fr = selectFeatures("classif.lda", task=multiclass.task, resampling=inner, bit.names=bns, bits.to.features=btf, control=ctrl)
  df = as.data.frame(fr$path) 
  expect_equal(colnames(df), c("b1", "b2", "mmce.test.mean", "dob", "eol"))
  expect_equal(nrow(df), 4)
})

