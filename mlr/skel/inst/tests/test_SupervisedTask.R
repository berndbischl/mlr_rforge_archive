context("SupervisedTask")

test_that("SupervisedTask", {
	ct1 = multiclass.task
	
	expect_equal(ct1$desc$target, "Species")
	expect_equal(getTargets(ct1), multiclass.df[,multiclass.target])
	
	ct = binaryclass.task
	pn = c(ct$desc$positive, ct$desc$negative)
	expect_equal(sort(ct$desc$class.levels), sort(pn))
	
  ct2 = subsetTask(ct, subset=1:150)
  expect_equal(ct$desc$positive, ct2$desc$positive)
  
	# wrong vars
	expect_error(subsetTask(multiclass.task, vars=c("Sepal.Length", "x", "y")), "VVV")
	
	# check missing accessors
	df = multiclass.df
	df[1,1:3] = NA
	df[2,1:3] = NA
	ct = makeClassifTask(data=df, target=multiclass.target)	
	expect_true(ct$desc$has.missing)
})
