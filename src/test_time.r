source("src/files.r")
load.all.libs()
load.all.sources("src")

logger.setup(level="warn")
parallel.setup(level="local")
errorhandler.setup()

set.seed(1)
x = mlbench.twonorm(10000, 10)
dd = as.data.frame(x$x)
dd$classes = x$classes
ct = make.task(target="classes", data=dd)
#Rprof()
#ts1 = replicate(10, system.time(m<-train("classif.lda", task=ct))[1])
#Rprof(NULL)
#print(mean(ts1))
#sr = summaryRprof()

wl=make.learner("classif.lda")
ts1 = replicate(20,
	system.time(learner.model <- train.learner(.learner=wl, .target="classes", .data=dd, .data.desc=ct@data.desc, .task.desc=ct@task.desc, .weights=rep(1,nrow(dd)), .costs=matrix(0,0,0)))[1]
)
print(mean(ts1))

ts1 = replicate(20, system.time(m<-train("classif.lda", task=ct))[1])
print(mean(ts1))


#ts2 = mean(replicate(10, system.time(m<-lda(x$x, x$classes))[1]))	
#ts3 = mean(replicate(10, system.time(m<-lda(classes~., data=dd))[1]))
#print(c(ts2, ts3))
