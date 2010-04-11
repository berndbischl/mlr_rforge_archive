


source("src/files.r")
load.all.libs()
load.all.sources("src")

parallel.setup(mode="local")
logger.setup(level="warn")


library(mlbench)
library(penalizedSVM)
data(PimaIndiansDiabetes)

x = sim.data(200, ng=10, nsg=2, sg.pos.factor=10,sg.neg.factor=-10)
df = as.data.frame(t(x$x))
df$y = as.factor(x$y)

ct = make.classif.task("sd", data=df, target="y")
#ct = make.classif.task("Pima", data=PimaIndiansDiabetes, target="diabetes")
#ct = make.classif.task("Pima", data=PimaIndiansDiabetes, target="diabetes")
#ct = make.classif.task("iris", data=iris, target="Species")

res = make.res.desc("cv", iter=2)

#vr = varsel(learner="qda", task=ct, resampling=res, method="sffs", control=varsel.control(alpha=0.001, beta=0))
vr = varsel(learner="qda", task=ct, resampling=res, method="hybrid", 
		control=varsel.control(alpha=0, beta=0, maxit=10))

#print("sol")
#print(vr)
