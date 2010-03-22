


library(mlr)
library(mlbench)
data(LetterRecognition, BreastCancer, Satellite)

ct = make.classif.task(data=Satellite, target="classes")
res = make.res.desc("cv", iters=10)
wl = make.learner("randomForest.classif", ntree=200)

parallel.setup(mode="local")
st = system.time({
	rf = resample.fit(wl, ct, res)
})
print(st)
print(performance(rf)$measures["mean",])


parallel.setup(mode="sfCluster", level="resample")
st = system.time({
			rf = resample.fit(wl, ct, res)
		})
print(st)
print(performance(rf)$measures["mean",])








