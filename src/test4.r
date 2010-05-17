

source("src/files.r")
load.all.libs()
load.all.sources("src")

logger.setup(level="debug")
parallel.setup(mode="local")



ct =  make.task(data=iris, target="Species")
fun = function(data, x, y) {
	data[,1] = data[,1]*x
	return(data)
}

wl = make.learner("classif.rpart", minsplit=7)
wl = make.preproc.wrapper(wl, fun=fun, x=1, y=2)
#print(wl["hyper.pars"])
#print(wl["hyper.types"])
#print(wl["hyper.names"])
#print(wl["hyper.pars", type="wrapper"])
#print(wl["hyper.names", type="wrapper"])
#print(wl["hyper.pars", type="train"])
#print(wl["hyper.names", type="train"])
m = train(wl, task=ct, parset=list(y=33, minsplit=44, cp=0.3))

