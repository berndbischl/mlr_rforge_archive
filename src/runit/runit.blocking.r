

test.blocking = function() {
	df = multiclass.df
	b = as.factor(rep(1:30, 5))	
	ct = make.task(target=multiclass.target, data=multiclass.df, blocking=b)
	checkTrue(ct["has.blocking"])
	res = make.res.instance("cv", iters=3, task=ct)
	for (j in 1:res["iters"]) {
		train.j = get.train.set(res, j)
		test.j = get.test.set(res, j)$inds
		tab = table(b[train.j])
		checkTrue(setequal(c(0,5), unique(as.numeric(tab))))
		tab = table(b[test.j])
		checkTrue(setequal(c(0,5), unique(as.numeric(tab))))
	}
}



