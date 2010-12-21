source("src/files.r")
load.all.libs()
load.all.sources("src")

logger.setup(level="info")
parallel.setup(mode="local")
errorhandler.setup()


library(mlbench)
data(Sonar)

mt = make.task(data = Sonar, target = "Class", positive = "R")
s1 = sample(1:mt["size"], 100)
s2 = setdiff(1:mt["size"], s1)
m = train("classif.rpart", task=mt, subset=s1)
p1 = predict(m, task=mt, subset=s1)
p2 = predict(m, task=mt, subset=s2)
performance(pred=p1, measure=mmce)
performance(p2, measure=mmce)
#performance(p2, pred.train=p1, measure=inbag(mmce))
#performance(p2, pred.train=p1, measure=oob(mmce))
#performance(p2, pred.train=p1, measure=b632(mmce))
performance(model=m, measure=nvars)
performance(model=m, measure=time.fit)
performance(p2, measure=time.predict)
performance(p2, model=m, measure=time.all)

cm = make.cost.measure(costs=matrix(c(0,1,2,0), 2, dimnames=list(c("M", "R"), c("M", "R"))), task=mt)
performance(p2, measure=cm)
print(conf.matrix(p2))
performance(p2, measure=auc)

m = train(make.learner("classif.rpart", predict.type="prob"), task=mt, subset=s1)
p3 = predict(m, task=mt, subset=s2)
performance(p3, measure=auc)




#acp = make.measure(
#  fun = un=function(pred.test, pred.train, model, task, pars) {
#    cat("mcflag: ", mean(data$mcFlag != mcFlag2), "\n")
#    cat("isK: ", mean(data$isK != isK2), "\n")
#    NDKp = sum(data$mcFlag==1 & data$isK==1 & data$charge==1)
#    NDKm = sum(data$mcFlag==1 & data$isK==1 & data$charge==-1)
#    NDK = NDKp + NDKm
#    NDpi = sum(data$mcFlag==1 & data$isK==2)
#    list(ACP=(NDKp-NDKm)/(NDKp+NDKm), RCP= NDK/NDpi)
#  }
#)







#inner <- make.res.desc("stratcv", iter=2)
#outer <- make.res.desc("subsample", iter=1, split = 4/5)
#
#control = grid.control(ranges = list(n.trees = c(100), interaction.depth = 1, distribution = "bernoulli",
#    shrinkage = c(0.001, 0.01)))
#wl <- make.tune.wrapper("classif.gbm", resampling = inner, control = control)
#
#mt <- make.task(data = Sonar, target = "Class", positive = "R")
#set.seed(5)
#be <- bench.exp(learners = wl, tasks = mt, resampling = outer, predictions=TRUE,
#  paths=TRUE, model = TRUE)
#print(be["perf"])
#print(be["conf.mat"])
#p=as.data.frame(be["predictions"][[1]][[1]])
