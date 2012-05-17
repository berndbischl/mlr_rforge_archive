context("TaskDesc")

test_that("TaskDesc", {
	ct = makeClassifTask(target="Class", binaryclass.df, id="mytask", positive="M", exclude="V1")
	expect_equal(ct$desc$id, "mytask")	
	expect_equal(ct$desc$positive, "M")	
	expect_equal(ct$desc$negative, "R")

	ct = makeClassifTask(target="Species", multiclass.df, id="mytask2")
	expect_equal(ct$desc$id, "mytask2")	
	expect_true(is.na(ct$desc$positive))
	expect_true(is.na(ct$desc$negative))
	
	rt = makeRegrTask(target="medv", regr.df, id="mytask3") 
	expect_equal(rt$desc$id, "mytask3")	
	expect_true(is.na(rt$desc$positive))
	expect_true(is.na(rt$desc$negative))
  
  expect_equal(multiclass.task$desc$size, 150) 
  expect_equal(sum(multiclass.task$desc$n.feat), 4)  
  expect_equal(multiclass.task$desc$n.feat[["numerics"]], 4)  
  expect_equal(multiclass.task$desc$n.feat[["factors"]], 0)  
  expect_equal(multiclass.task$desc$has.missing, F)  
  expect_equal(multiclass.task$desc$type, "classif") 
  expect_equal(multiclass.task$desc$class.levels, c("setosa", "versicolor", "virginica"))  
  
  # check missing values
  df = multiclass.df
  df[1,1] = as.numeric(NA)
  ct = makeClassifTask(target="Species", data=df)
  expect_equal(ct$desc$has.missing, T) 
  
  ct = makeClassifTask(target=binaryclass.target, data=binaryclass.df, exclude="V1")
  expect_equal(ct$desc$size, 208)  
  expect_equal(sum(ct$desc$n.feat), 59)  
  expect_equal(ct$desc$n.feat[["numerics"]], 59)  
  expect_equal(ct$desc$n.feat[["factors"]], 0)  
  expect_equal(ct$desc$has.missing, F) 
  expect_equal(ct$desc$type, "classif")  
  expect_equal(ct$desc$class.levels, c("M", "R"))  
  
  expect_equal(regr.task$desc$size, 506) 
  expect_equal(sum(regr.task$desc$n.feat), 13) 
  expect_equal(regr.task$desc$n.feat[["numerics"]], 12)  
  expect_equal(regr.task$desc$n.feat[["factors"]], 1)  
  expect_equal(regr.task$desc$has.missing, F)  
  expect_equal(regr.task$desc$type, "regr")  
  expect_true(is.na(regr.task$desc$class.levels)) 
})