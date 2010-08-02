#'	\item{target.name [string]}{The name of the target variable.}
#' }

test.task.desc <- function() {
	costs = matrix(1:4, 2, 2)
	rownames(costs) = colnames(costs) = c("M", "R") 
	ct = make.task(target="Class", binaryclass.df, id="mytask", label="lab", costs=costs, 
			positive="M", excluded="V1")
	checkEquals(ct["id"], "mytask")	
	checkEquals(ct["label"], "lab")	
	checkEquals(ct["costs"], costs)	
	checkEquals(ct["positive"], "M")	
	checkEquals(ct["negative"], "R")

	ct = make.task(target="Species", multiclass.df, id="mytask2", label="lab2")
	checkEquals(ct["id"], "mytask2")	
	checkEquals(ct["label"], "lab2")	
	checkEquals(dim(ct["costs"]), c(0,0))	
	checkTrue(is.na(ct["positive"]))
	checkTrue(is.na(ct["negative"]))
	
	rt = make.task(target="medv", regr.df, id="mytask3", label="lab3") 
	checkEquals(rt["id"], "mytask3")	
	checkEquals(rt["label"], "lab3")	
	checkEquals(dim(rt["costs"]), c(0,0))	
	checkTrue(is.na(rt["positive"]))
	checkTrue(is.na(rt["negative"]))
}