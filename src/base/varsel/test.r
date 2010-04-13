


source("src/files.r")
load.all.libs()
load.all.sources("src")

parallel.setup(mode="local")
logger.setup(level="warn")


library(mlbench)
library(penalizedSVM)
data(PimaIndiansDiabetes)

#x = sim.data(200, ng=10, nsg=2, sg.pos.factor=10,sg.neg.factor=-10)
#df = as.data.frame(t(x$x))
#df$y = as.factor(x$y)

#ct = make.task("sd", data=df, target="y")
ct = make.task("Pima", data=PimaIndiansDiabetes, target="diabetes")
#ct = make.task("iris", data=iris, target="Species")

res = make.res.desc("cv", iter=2)

ctrl = varsel.control(maxit=40, alpha=0.005, beta=0, gamma=0, delta=0.1)
method = "hybrid"
vr = varsel(learner="classif.qda", task=ct, resampling=res, method=method, control=ctrl) 


pp = function(vr) {
	path = vr$path
	p = vr$opt
	v = paste(p$vars, collapse = "/")
	print(paste(v, p$perf[1], p$eval, p$event))
	cat("\n")
	for (i in 1:length(path)) {
		p = path[[i]]
		v = paste(p$vars, collapse = "/")
		print(paste(v, format(p$perf[1], digits=3), p$eval, p$event, p$accept))
	}
}

pp(vr)

#print("sol")
#print(vr)
