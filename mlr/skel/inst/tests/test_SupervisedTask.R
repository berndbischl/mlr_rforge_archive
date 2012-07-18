context("SupervisedTask")

test_that("SupervisedTask", {
  ct1 = multiclass.task
  
  expect_equal(ct1$task.desc$target, "Species")
  expect_equal(getTaskTargets(ct1), multiclass.df[,multiclass.target])
  
  ct = binaryclass.task
  pn = c(ct$task.desc$positive, ct$task.desc$negative)
  expect_equal(sort(ct$task.desc$class.levels), sort(pn))
  
  ct2 = subsetTask(ct, subset=1:150)
  expect_equal(ct$task.desc$positive, ct2$task.desc$positive)
  
  # wrong vars
  expect_error(subsetTask(multiclass.task, features=c("Sepal.Length", "x", "y")))
  
  # check missing accessors
  df = multiclass.df
  df[1,1:3] = NA
  df[2,1:3] = NA
  ct = makeClassifTask(data=df, target=multiclass.target)  
  expect_true(ct$task.desc$has.missing)  
})
