source("src/files.r")
load.all.libs()
load.all.sources("src")

logger.setup(level="warn")
parallel.setup(level="local")

library(mlbench)
data(Sonar)

source("D:/sync/thesis/meinke/src/imbalance.wrapper.r")

ct = make.task(data=Sonar, target="Class")
res = make.res.desc("cv", iters=10)

wl1 = make.learner("classif.logreg", id="lr1", predict.type="prob")
wl1a = make.imbalance.wrapper(wl1, id="lr2", method="under")
wl2 = make.learner("classif.lda", id="lda1", predict.type="prob")
wl2a = make.imbalance.wrapper(wl2, id="lda2", method="under")
wl3 = make.learner("classif.rpart", id="rp1", predict.type="prob")
wl3a = make.imbalance.wrapper(wl3, id="rp2", method="under")
learners = list(wl1, wl1a, wl2, wl2a, wl3, wl3a)

ms = c("mmce", "tp", "tpr", "fp", "fpr", "tn", "tnr", "fn", "fnr", "ppv", "npv", "fdr", "f1", "mcc", "gmean", "gpr")
#ms = c("mmce")

be = bench.exp(tasks=ct, learners=learners, resampling=res, measures=ms, predictions=TRUE, models=TRUE)


