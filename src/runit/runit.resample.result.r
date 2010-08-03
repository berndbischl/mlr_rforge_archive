

test.resample.prediction = function() {
	rin1 <- make.res.instance("bs", multiclass.task, iters=4)
	rin2 <- make.res.instance("cv", multiclass.task, iters=7)
	rin3 <- make.res.instance("subsample", multiclass.task, iters=2)
	
	ct <- make.task(data=multiclass.df, target=multiclass.target)
	
	result1 <- resample.fit("classif.lda", ct, rin1)       
	result2 <- resample.fit("classif.lda", ct, rin2)       
	result3 <- resample.fit("classif.lda", ct, rin3)       
	
	checkEquals(result1["iters"], 4)
	checkEquals(result2["iters"], 7)
	checkEquals(result3["iters"], 2)

	checkEquals(result3["iter"], rep(c(1,2), each=50))
	
	cc <- ct["target.col"]
	
#	for (i in 1:rin1["iters"]) {
#		checkEquals(rin1["train.inds", i], result1["train.inds", i])
#		checkEquals(rin1["test.inds", i], result1["test.inds", i])
#		checkEquals(result1["targets.train", i], multiclass.df[result1["train.inds", i], cc])
#		checkEquals(result1["targets.test", i], multiclass.df[result1["test.inds", i], cc])
#	}		
#		
#	for (i in 1:rin2["iters"]) {
#		checkEquals(rin2["train.inds", i], result2["train.inds", i])
#		checkEquals(rin2["test.inds", i],  result2["test.inds", i])
#		checkEquals(result2["targets.train", i], multiclass.df[result2["train.inds", i], cc])
#		checkEquals(result2["targets.test", i], multiclass.df[result2["test.inds", i], cc])
#	}		
#		
#	for (i in 1:rin3["iters"]) {
#		checkEquals(rin3["train.inds", i], result3["train.inds", i])
#		checkEquals(rin3["test.inds", i],  result3["test.inds", i])
#		checkEquals(result3["targets.train", i], multiclass.df[result3["train.inds", i], cc])
#		checkEquals(result3["targets.test", i], multiclass.df[result3["test.inds", i], cc])
#	}		
}



