
test.task.desc <- function() {
	ct = makeClassifTask(target="Class", binaryclass.df, id="mytask", positive="M", exclude="V1")
	checkEquals(ct@desc@id, "mytask")	
	checkEquals(ct@desc@positive, "M")	
	checkEquals(ct@desc@negative, "R")

	ct = makeClassifTask(target="Species", multiclass.df, id="mytask2")
	checkEquals(ct@desc@id, "mytask2")	
	checkTrue(is.na(ct@desc@positive))
	checkTrue(is.na(ct@desc@negative))
	
	rt = makeRegrTask(target="medv", regr.df, id="mytask3") 
	checkEquals(rt@desc@id, "mytask3")	
	checkTrue(is.na(rt@desc@positive))
	checkTrue(is.na(rt@desc@negative))
  
  checkEquals(multiclass.task@desc@size, 150) 
  checkEquals(sum(multiclass.task@desc@n.feat), 4)  
  checkEquals(multiclass.task@desc@n.feat["numerics"], 4, checkNames=FALSE)  
  checkEquals(multiclass.task@desc@n.feat["integers"], 0, checkNames=FALSE)  
  checkEquals(multiclass.task@desc@n.feat["factors"], 0, checkNames=FALSE)  
  checkEquals(multiclass.task@desc@n.feat["logicals"], 0, checkNames=FALSE)  
  checkEquals(multiclass.task@desc@n.feat["characters"], 0, checkNames=FALSE)  
  checkEquals(multiclass.task@desc@has.missing, F)  
  checkEquals(multiclass.task@desc@type, "classif") 
  checkEquals(multiclass.task@desc@class.levels, c("setosa", "versicolor", "virginica"))  
  
  # check missing values
  df = multiclass.df
  df[1,1] = as.numeric(NA)
  ct = makeClassifTask(target="Species", data=df)
  checkEquals(ct@desc@has.missing, T) 
  
  ct = makeClassifTask(target=binaryclass.target, data=binaryclass.df, exclude="V1")
  checkEquals(ct@desc@size, 208)  
  checkEquals(sum(ct@desc@n.feat), 59)  
  checkEquals(ct@desc@n.feat["numerics"], 59, checkNames=FALSE)  
  checkEquals(ct@desc@n.feat["integers"], 0, checkNames=FALSE)  
  checkEquals(ct@desc@n.feat["factors"], 0, checkNames=FALSE)  
  checkEquals(ct@desc@n.feat["logicals"], 0, checkNames=FALSE)  
  checkEquals(ct@desc@n.feat["characters"], 0, checkNames=FALSE)  
  checkEquals(ct@desc@has.missing, F) 
  checkEquals(ct@desc@type, "classif")  
  checkEquals(ct@desc@class.levels, c("M", "R"))  
  
  checkEquals(regr.task@desc@size, 506) 
  checkEquals(sum(regr.task@desc@n.feat), 13) 
  checkEquals(regr.task@desc@n.feat["numerics"], 12, checkNames=FALSE)  
  checkEquals(regr.task@desc@n.feat["integers"], 0, checkNames=FALSE)  
  checkEquals(regr.task@desc@n.feat["factors"], 1, checkNames=FALSE)  
  checkEquals(regr.task@desc@n.feat["logicals"], 0, checkNames=FALSE)  
  checkEquals(regr.task@desc@n.feat["characters"], 0, checkNames=FALSE)  
  checkEquals(regr.task@desc@has.missing, F)  
  checkEquals(regr.task@desc@type, "regr")  
  checkTrue(is.na(regr.task@desc@class.levels)) 
}