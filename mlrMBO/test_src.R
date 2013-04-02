library(methods)
library(testthat)

  library(devtools)
  load_all("skel")

configureMlr(show.learner.output=FALSE)


#set.seed(2)
#f1 = makeMBOFunction(function(x) 1)

task = makeClassifTask(data = iris, target = "Species")
rin = makeResampleInstance(makeResampleDesc("CV", iters = 5), task = task)

obj = function(...) {
  pars = as.list(...)
  lrn = makeLearner("classif.ksvm", par.vals = pars)
  resample(lrn, task, rin, show.info = FALSE)$aggr[1]
}

ps = makeParamSet(
  makeNumericParam("C", lower=-15, upper=15, trafo = function(x) 2^x),
  makeNumericParam("sigma", lower=-15, upper=15, trafo = function(x) 2^x)
)

#surrogate = makeLearner("regr.randomForest", ntree = 50)
surrogate = makeLearner("regr.km", nugget.estim = TRUE)

ctrl = makeMBOControl(seq.loops = 10, init.design.points = 12, 
  propose.points.method = "EI",
  final.point = "best.predicted"
)
res = mbo(obj, ps, des=NULL, surrogate, ctrl)
print(res$x)
print(res$y)

library(ggplot2)
mod = res$models[[length(res$models)]]
path = as.data.frame(res$path)
path$y2 = predict(mod, newdata = path[,1:2])$data$response
#des = expand.grid(C = s, sigma = s)
des = generateDesign(5000, ps)
des$response = predict(mod, newdata = des)$data$response
des2 = interp(des$C, des$sigma, des$response)
image(des2$x, des2$y, des2$z)
contour(des2$x, des2$y, des2$z, add = TRUE)
#oints(path$C, path$sigma)
text(path$C, path$sigma)
points(res$x$C, res$x$sigma, pch = 21, cex = 3)
#k = sqrt(nrow(des)) / 2
#quilt.plot(des$C, des$sigma, des$response, nx = k, ny = k, co)
#des.m = melt(des, id.vars = 1:3)
# print(
#   ggplot(des2, aes(C, sigma, z = response, fill = response)) +
#   geom_tile()    
#   #geom_contour()
# )  