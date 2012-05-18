context("getTaskData")

test_that("getTaskData", {
  df = getTaskData(multiclass.task)
  expect_equal(df, multiclass.df)
  df = getTaskData(multiclass.task, subset=1:10, features=colnames(multiclass.df)[1:2])
  expect_equal(df, multiclass.df[1:10, 1:2])
  
  # class.as
  df = getTaskData(binaryclass.task, class.as="01")
  expect_equal(df[, 1:20], binaryclass.df[, 1:20])
  expect_true(is.numeric(df[, binaryclass.target]))
  expect_equal(sum(df[, binaryclass.target] == 1), 
    sum(binaryclass.df[, binaryclass.target] == binaryclass.task$task.desc$positive))
  expect_equal(sum(df[, binaryclass.target] == 0), 
    sum(binaryclass.df[, binaryclass.target] == binaryclass.task$task.desc$negative))
  df = getTaskData(binaryclass.task, class.as="-1+1")
  expect_equal(df[,1:20], binaryclass.df[, 1:20])
  expect_true(is.numeric(df[, binaryclass.target]))
  expect_equal(sum(df[, binaryclass.target] == 1), 
    sum(binaryclass.df[, binaryclass.target] == binaryclass.task$task.desc$positive))
  expect_equal(sum(df[, binaryclass.target] == -1), 
    sum(binaryclass.df[, binaryclass.target] == binaryclass.task$task.desc$negative))
	# FIXME include
  #expect_error(getTaskData(binaryclass.task, class.as="foo"), "Argument class.as must be any of")
  
  x = getTaskData(multiclass.task, target.extra=TRUE)
  expect_equal(x$data[,1:4], multiclass.df[,1:4])
  expect_equal(x$target, multiclass.df[, multiclass.target])
})