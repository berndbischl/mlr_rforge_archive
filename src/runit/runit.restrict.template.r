
test.restrict.learn.task = function() {
	
	ct <- make.task(data=multiclass.df, formula=multiclass.target)
	
	inds2 = c(1,7,10)
	inds3 = c(1,1,7,11,11)
	
	ct2 <- restrict.learn.task(ct, inds2)
	ct3 <- restrict.learn.task(ct, inds3)
	
	checkEquals(model.frame(multiclass.formula, multiclass.df[inds2,])[,1:5], ct2["data"])
	checkEquals(model.frame(multiclass.formula, multiclass.df[inds3,])[,1:5], ct3["data"])
}
