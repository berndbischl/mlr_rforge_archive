context("resample: stratification")

test_that("stratification instances work", {
  
  mytest = function(rin, size1, size2)  {
    for (i in 1:rin$desc$iters) {
      i1 = rin$train.inds[[i]]
      i2 = rin$test.inds[[i]]
      if (!missing(size1))
        expect_true(all(as.numeric(table(getTaskTargets(multiclass.task)[i1])) == size1)) 
      if (!missing(size2))
        expect_true(all(as.numeric(table(getTaskTargets(multiclass.task)[i2])) == size2)) 
      expect_equal(sort(c(unique(i1), i2)), 1:150)
    }
  }
  
  expect_error(makeResampleDesc("LOO", stratify=TRUE), "Stratification cannot")
  
  rin = makeResampleInstance(makeResampleDesc("Holdout", stratify=TRUE), task=multiclass.task)  
  mytest(rin, 33, 17)  

  rin = makeResampleInstance(makeResampleDesc("Subsample", iters=3, split=0.5, stratify=TRUE), 
    task=multiclass.task)  
  mytest(rin, 25, 25)  

  rin = makeResampleInstance(makeResampleDesc("CV", iters=10, stratify=TRUE), task=multiclass.task)  
  mytest(rin, 45, 5)  

  rin = makeResampleInstance(makeResampleDesc("RepCV", reps=2, folds=5, stratify=TRUE), task=multiclass.task)  
  mytest(rin, 40, 10)  

  rin = makeResampleInstance(makeResampleDesc("Bootstrap", iters=1, stratify=TRUE), task=multiclass.task)  
  mytest(rin, 50)  
})



