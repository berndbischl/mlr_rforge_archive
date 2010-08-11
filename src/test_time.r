source("src/files.r")
load.all.libs()
load.all.sources("src")

logger.setup(level="warn")
parallel.setup(level="local")
errorhandler.setup()

set.seed(1)
x = mlbench.twonorm(2000, 3)
dd = as.data.frame(x$x)
dd$classes = x$classes
ct = make.task(target="classes", data=dd)
#Rprof()
ts1 = replicate(100, system.time(m<-train("classif.lda", task=ct))[1])
#Rprof(NULL)
print(mean(ts1))
#sr = summaryRprof()

ts2 = mean(replicate(100, system.time(m<-lda(x$x, x$classes))[1]))	
ts3 = mean(replicate(100, system.time(m<-lda(classes~., data=dd))[1]))
print(c(ts2, ts3))