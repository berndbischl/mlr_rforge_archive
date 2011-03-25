test.mlr.learn.task <- function() {
	
	ct1 <- multiclass.task
	
	checkEquals(ct1@desc@target, "Species")
	checkEquals(getTargets(ct1), multiclass.df[,multiclass.target])
	
	ct = binaryclass.task
	pn = c(ct["positive"], ct["negative"])
	checkEquals(sort(getClassLevels(ct)), sort(pn))
	
	# wrong vars
	x=checkException(
			train("classif.lda", multiclass.task, vars=c("Sepal.Length", "x", "y")),		 
			silent=TRUE
	)
	
	# y contains missings
	df = multiclass.df
	df[1, multiclass.target] = NA
	checkException(makeClassifTask(data=df, target=multiclass.target), silent=TRUE)
	s = geterrmessage()
	checkTrue(length(grep("Target values contain missings!", s)) >0 )

	# y contains infs
	df = regr.df
	df[1, regr.target] = Inf
	checkException(makeRegrTask(data=df, target=regr.target), silent=TRUE)
	s = geterrmessage()
	checkTrue(length(grep("Target values contain inf", s)) >0 )
	
	# check missing accessors
	df = multiclass.df
	df[1,1:3] = NA
	df[2,1:3] = NA
	ct = makeClassifTask(data=df, target=multiclass.target)	
	checkTrue(ct["has.missing"])
  
  #	check inf accessors
	df = multiclass.df
	df[1,1:3] = Inf
	df[2,1:3] = Inf
	ct = makeClassifTask(data=df, target=multiclass.target)	
	checkTrue(!ct["has.inf"])
  
  # check logical and character targets
  df = multiclass.df
  df[, multiclass.target] = as.character(df[, multiclass.target]) 
  ct = makeClassifTask(data=df, target=multiclass.target) 
  df = multiclass.df
  df[, multiclass.target] = rep(TRUE, nrow(multiclass.df)) 
  ct = makeClassifTask(data=df, target=multiclass.target) 
}