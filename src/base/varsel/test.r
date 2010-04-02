


source("src/files.r")
load.all.libs()
load.all.sources("src")

parallel.setup(mode="local")
logger.setup(level="warn")


library(mlbench)
data(PimaIndiansDiabetes)

#ct = make.classif.task("Pima", data=PimaIndiansDiabetes, target="diabetes")
ct = make.classif.task("iris", data=iris, target="Species")

res = make.res.desc("cv", iter=10)


vr = varsel(learner="qda", task=ct, resampling=res, method="sffs", control=sel.control(alpha=0.001, beta=0))

print("sol")
print(vr)
