## FIXME: selectFeaturesGA does not work for bits
context("selectFeatures")

test_that("selectFeatures", {
  inner = makeResampleDesc("CV", iter=2)
  lrn = makeLearner("classif.lda")
	
  # check all methods
  ctrl = makeFeatSelControl(max.features=2, cl="Exhaustive")
  fr = selectFeatures(lrn, task=multiclass.task, resampling=inner, control=ctrl, show.info=FALSE)
  expect_equal(getOptPathLength(fr$opt.path), 11) 
  expect_equal(nrow(as.data.frame(fr$opt.path)), 11) 
  expect_equal(ncol(as.data.frame(fr$opt.path)), 7) 
  # test printing
  print(fr)

  # check maxit
  ctrl = makeFeatSelControl(maxit=4, cl="Random")
  fr = selectFeatures(lrn, task=multiclass.task, resampling=inner, control=ctrl, show.info=FALSE)
  expect_equal(getOptPathLength(fr$opt.path), 4) 
  
  ctrl = makeFeatSelControl(method="sfs", alpha=0.01, cl="Sequential")
  fr = selectFeatures(lrn, task=multiclass.task, resampling=inner, control=ctrl, show.info=FALSE)
  expect_true(getOptPathLength(fr$opt.path) > 1) 
  
  ctrl = makeFeatSelControl(method="sbs", beta=0.01, cl="Sequential")
  fr = selectFeatures(lrn, task=multiclass.task, resampling=inner, control=ctrl, show.info=FALSE)
  expect_true(getOptPathLength(fr$opt.path) > 1) 
  
  ctrl = makeFeatSelControl(method="sffs", alpha=0.01, cl="Sequential")
  fr = selectFeatures(lrn, task=multiclass.task, resampling=inner, control=ctrl, show.info=FALSE)
  # we must at least try to select a 2nd feature
  expect_true(getOptPathLength(fr$opt.path) >= 1 + 4 + 1 + 3) 
  
  ctrl = makeFeatSelControl(method="sfbs", beta=0.01, cl="Sequential")
  fr = selectFeatures(lrn, task=multiclass.task, resampling=inner, control=ctrl, show.info=FALSE)
  expect_true(getOptPathLength(fr$opt.path) > 1) 
  
  
  # check max.features
  ctrl = makeFeatSelControl(alpha=0, max.features=1, method="sfs", cl="Sequential")
  fr = selectFeatures(lrn, task=binaryclass.task, resampling=inner, control=ctrl, show.info=FALSE)
  expect_equal(length(fr$x), 1) 
  
  ctrl = makeFeatSelControl(beta=1, max.features=58, method="sbs", cl="Sequential")
  fr = selectFeatures(lrn, task=binaryclass.task, resampling=inner, control=ctrl, show.info=FALSE)
  expect_equal(length(fr$x), 58) 
  
  ctrl = makeFeatSelControl(maxit=5, max.features=30, cl="GA")
  fr = selectFeatures(lrn, task=binaryclass.task, resampling=inner, control=ctrl, show.info=FALSE)
  expect_true(length(fr$x) <= 30)
  
  # check empty model
  ctrl = makeFeatSelControl(method="sfs", alpha=10, cl="Sequential")
  fr = selectFeatures(lrn, task=multiclass.task, resampling=inner, control=ctrl, show.info=FALSE)
  expect_equal(fr$x, character(0)) 
  
  # check bits
  bns = c("b1", "b2")
  btf = function(x, task) {
    fns = getTaskFeatureNames(task)
    Reduce(c, list(fns[1:2], fns[3:4])[as.logical(x)], init=character(0))
  } 
  
  ctrl = makeFeatSelControl(maxit=3, cl="Random")
  fr = selectFeatures(lrn, task=multiclass.task, resampling=inner, bit.names=bns, bits.to.features=btf, control=ctrl, show.info=FALSE)
  df = as.data.frame(fr$opt.path) 
  expect_equal(colnames(df), c("b1", "b2", "mmce.test.mean", "dob", "eol"))
  expect_equal(nrow(df), 3)
  
  ctrl = makeFeatSelControl(cl="Exhaustive")
  fr = selectFeatures(lrn, task=multiclass.task, resampling=inner, bit.names=bns, bits.to.features=btf, control=ctrl, show.info=FALSE)
  df = as.data.frame(fr$opt.path) 
  expect_equal(colnames(df), c("b1", "b2", "mmce.test.mean", "dob", "eol"))
  expect_equal(nrow(df), 4)
  
  ctrl = makeFeatSelControl(method="sfs", alpha=0, cl="Sequential")
  fr = selectFeatures(lrn, task=multiclass.task, resampling=inner, bit.names=bns, bits.to.features=btf, control=ctrl, show.info=FALSE)
  df = as.data.frame(fr$opt.path) 
  expect_equal(colnames(df), c("b1", "b2", "mmce.test.mean", "dob", "eol"))
  expect_equal(nrow(df), 4)
  
  ctrl = makeFeatSelControl(maxit=5, lambda = 6, mu = 15, cl="GA")
  fr = selectFeatures(lrn, task=multiclass.task, resampling=inner, bit.names=bns, bits.to.features=btf, control=ctrl, show.info=FALSE)
  df = as.data.frame(fr$opt.path) 
  expect_equal(colnames(df), c("b1", "b2", "mmce.test.mean", "dob", "eol"))
  expect_equal(nrow(df), 15 + 5 * 6)
})

