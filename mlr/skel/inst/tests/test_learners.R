context("learners")

test_that("listLearners", {
  x1 = listLearners()
  x2 = listLearners(type="classif") 
  x3 = listLearners(type="regr") 
	expect_true(length(x1) > 40)
	expect_true(length(x2) > 10)
	expect_true(length(x2) > 10)
  expect_true(setequal(x1, union(x2, x3)))
  
  x4 = listLearners(type="classif", multiclass=TRUE, factors=TRUE, prob=TRUE) 
  expect_true(length(x4) > 10 && all(x4 %in% x2))
})

test_that("listLearnersForTask", {
  x1 = listLearnersForTask(task=binaryclass.task) 
  x2 = listLearnersForTask(task=multiclass.task) 
  x3 = listLearnersForTask(task=regr.task) 
  expect_true(length(x1) > 10)
  expect_true(length(x2) > 10)
  expect_true(length(x3) > 10)
  expect_true(length(intersect(x1, x3)) == 0)
  expect_true(length(intersect(x2, x3)) == 0)
  expect_true(all(x2 %in% x1))
})

test_that("learners work", {
  task = subsetTask(binaryclass.task, subset=c(1:50, 150:208), 
    features=getTaskFeatureNames(binaryclass.task)[1:2])
  lrns = listLearnersForTask(task=task) 
  lrns = lapply(lrns, makeLearner)
  lapply(lrns, function(lrn) {
    m = train(lrn, task)
    p = predict(m, task)
  })
  task = subsetTask(regr.task, subset=c(1:70),
    features=getTaskFeatureNames(regr.task)[1:2])
  lrns = listLearnersForTask(task=task) 
  lrns = lapply(lrns, makeLearner)
  lapply(lrns, function(lrn) {
    print(lrn$id)
    if (lrn$id == "regr.km")
      lrn = setHyperPars(lrn, nugget.estim=TRUE)
    m = train(lrn, task)
    p = predict(m, task)
  })
})
