test.mlr.learn.task <- function() {
	
	ct1 <- multiclass.task
	
	checkEquals(ct1["target"], "Species")
	checkEquals(ct1["targets"], multiclass.df[,multiclass.target])
#	mf = model.frame(multiclass.formula, multiclass.df)[,1:5]
#	checkEquals(ct1["data"], mf)
	
	
}