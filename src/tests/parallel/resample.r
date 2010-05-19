


library(mlr)
library(mlbench)
data(LetterRecognition, BreastCancer, Satellite)

ct = make.task(data=Satellite, target="classes")
res = make.res.desc("cv", iters=10)
wl = make.learner("classif.randomForest", ntree=200)

parallel.setup(mode="local")
st1 = system.time({
	rf1 = resample.fit(wl, ct, res)
})


parallel.setup(mode="sfCluster", level="resample")
st2 = system.time({
	rf2 = resample.fit(wl, ct, res)
})


print(st1)
print(performance(rf1)$measures["mean",])
#user  system elapsed
#64.568   1.188  65.775

print(st2)
print(performance(rf2)$measures["mean",])
#user  system elapsed
#0.616   0.044   9.594








