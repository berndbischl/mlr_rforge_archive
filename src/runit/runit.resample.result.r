

test.resample.prediction = function() {
	rin1 <- make.res.instance("bs", multiclass.task, iters=4)
	rin2 <- make.res.instance("cv", multiclass.task, iters=7)
	rin3 <- make.res.instance("subsample", multiclass.task, iters=2)
	
	p1 = resample("classif.lda", multiclass.task, rin1)       
	p2 = resample("classif.lda", multiclass.task, rin2)       
	p3 = resample("classif.lda", multiclass.task, rin3)       
	
	checkEquals(p1["iters"], 4)
	checkEquals(p2["iters"], 7)
	checkEquals(p3["iters"], 2)

	checkEquals(p3["iter"], rep(c(1,2), each=50))
	
	inds = Reduce(c, sapply(1:p1["iters"], function(i) get.test.set(rin1, i)$inds))
	y = multiclass.task["targets", row=inds]
	checkEquals(p1@df$id, inds)
	checkEquals(p1@df$truth, y)
	inds = Reduce(c, sapply(1:p2["iters"], function(i) get.test.set(rin2, i)$inds))
	y = multiclass.task["targets", row=inds]
	checkEquals(p2@df$id, inds)
	checkEquals(p2@df$truth, y)
	inds = Reduce(c, sapply(1:p3["iters"], function(i) get.test.set(rin3, i)$inds))
	y = multiclass.task["targets", row=inds]
	checkEquals(p3@df$id, inds)
	checkEquals(p3@df$truth, y)
}



