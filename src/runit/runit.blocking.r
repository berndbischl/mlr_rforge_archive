

test.blocking = function() {
	df = multiclass.df
	b = as.factor(rep(1:30, 5))	
	ct = make.task(target=multiclass.target, data=multiclass.df, blocking=b)
	checkTrue(ct["has.blocking"])
	res = make.res.instance("cv", iters=3, task=ct)
	for (j in 1:res["iters"]) {
		train.j = res["train.inds"][[j]]
		test.j = res["train.inds"][[j]]
		tab = table(b[train.j])
		checkTrue(setequal(c(0,5), unique(as.numeric(tab))))
		tab = table(b[test.j])
		checkTrue(setequal(c(0,5), unique(as.numeric(tab))))
	}
	# test blocking in resample
	res = make.res.desc("cv", iters=3)
	p = resample("classif.lda", ct, res)$pred
	for (j in 1:res["iters"]) {
		test.j = p@df[p@df$iter == j, "id"]
		tab = table(b[test.j])
		checkTrue(setequal(c(0,5), unique(as.numeric(tab))))
	}
	
	# test blocking in bench.exp
	be = bench.exp(tasks=ct, learners="classif.lda", resampling=res)
	p = be["prediction", learner="classif.lda"]
	res2 = be@resamplings[[1]]
	for (j in 1:res2["iters"]) {
		test.j = p@df[p@df$iter == j, "id"]
		tab = table(b[test.j])
		checkTrue(setequal(c(0,5), unique(as.numeric(tab))))
	}
	
}



