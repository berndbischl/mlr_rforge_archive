test.mlr.learn.task <- function() {
	data = multiclass.df
	formula = multiclass.formula
	inds <- multiclass.train.inds
	
#------------------------------lda----------------------------------------------
	
	ct1 <- make.classif.task(data=data, formula=formula)
	
	checkEquals(ct1["target"], "Species")
	checkEquals(ct1["targets"], data$Species)
	mf = model.frame(formula, data)[,1:5]
	checkEquals(ct1["data"], mf)
	
	
}