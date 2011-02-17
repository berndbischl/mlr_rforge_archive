

test.blocking = function() {
	df = multiclass.df
	b = as.factor(rep(1:30, 5))	
	ct = makeClassifTask(target=multiclass.target, data=multiclass.df, blocking=b)
	checkTrue(ct["has.blocking"])
	res = makeResampleInstance(makeResampleDesc("CV", iters=3), task=ct)
	for (j in 1:res@desc@iters) {
		train.j = res["train.inds"][[j]]
		test.j = res["test.inds"][[j]]
		tab = table(b[train.j])
		checkTrue(setequal(c(0,5), unique(as.numeric(tab))))
		tab = table(b[test.j])
		checkTrue(setequal(c(0,5), unique(as.numeric(tab))))
	}
	# test blocking in resample
	res = makeResampleDesc("CV", iters=3)
	p = resample("classif.lda", ct, res)$pred
	for (j in 1:res@iters) {
		test.j = p@df[p@df$iter == j, "id"]
		tab = table(b[test.j])
		checkTrue(setequal(c(0,5), unique(as.numeric(tab))))
	}
	
	# test blocking in bench.exp
	be = bench.exp(tasks=ct, learners="classif.lda", resampling=res)
	p = be["predictions"][[1]][[1]]
	res2 = be@resamplings[[1]]
	for (j in 1:res2@desc@iters) {
		test.j = p@df[p@df$iter == j, "id"]
		tab = table(b[test.j])
		checkTrue(setequal(c(0,5), unique(as.numeric(tab))))
	}
	
}



