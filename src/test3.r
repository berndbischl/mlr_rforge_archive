source("src/files.r")
load.all.libs()
load.all.sources("src")

logger.setup(level="warn")
parallel.setup(mode="local")

library(mlbench)
data(Sonar)

ct = make.task(data=Sonar, target="Class")

#tune chain
wl = make.learner("classif.rpart", minsplit=10, cp=0.01, predict.type="prob")

fun = function(data, target="Class", n) {
	cns2 = colnames(data)
	set.seed(1)
	cns = setdiff(cns2, target)
	cns = sample(cns, n)
	if (target %in% cns2)
		cns = c(cns, target)
	data[,cns, drop=F]
}
wl = make.preproc.wrapper(wl, fun=fun, n=3)

r = list(minsplit=c(3,30,300), n=c(1,10,60))
ctrl = grid.control(ranges=r, tune.threshold=T)
res = make.res.desc("cv", iter=5)
tr = tune(wl, ct, res, control=ctrl)
print(tr)
print(tr["path", as.data.frame=T])

