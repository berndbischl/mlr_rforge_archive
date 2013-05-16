# Mean response infill criterion.
# 
# @param points [\code{data.frame}]\cr 
#   Infill criterion function.
# @param model [\code{\link{WrappedModel}}]\cr
#   Model used for prediction.
# @return Mean response infill criterion.
infillCritMeanResponse = function(points, model) {
  predict(model, newdata=points)$data$response
}

infillCritNaiveEI = function(points, model) {
  p = predict(model, newdata=points)$data
  p$response - 3 * p$se
}

lrn=makeLearner("regr.km", covtype="powexp", nugget.estim=T, predict.type="se")
model=train(lrn, task)

infillCritMeanResponse = function(points, model) {
  predict(model, newdata=points)$data$response
}

infillCritStupidEI = function(points, model) {
  p = predict(model, newdata=points)$data
  p$response - 3 * p$se
}


infillCritEI = function(points, model){
  ei.vals=apply(des, 1,EI, model=model$learner.model)
  ei.vals
}

infillCritAEI = function(points, design, model, ctrl=NULL) {
  new
  pred = predict(model, newdata = design)$data
  qk <- pred$response + qnorm(0.75) * pred$se
  y.min <- pred$response[which.min(qk)]
  pred = predict(model, newdata = points)$data
  mk <- pred$response
  sk <- pred$se
  xcr <- (y.min - mk)/sk
  xcr.prob <- pnorm(xcr)
  xcr.dens <- dnorm(xcr)
  #if (sk < sqrt(model@covariance@sd2)/1e+06) {
  #FIXME: What actually happens here. Find out in DiceOptim
  if (sk < 1e-06) {
    aei.val <- 0
  } else {
    aei.val <- ((y.min - mk) * xcr.prob + sk * xcr.dens) * 
      (1 - sqrt(new.noise.var)/sqrt(new.noise.var + sk^2))
  }
  return(aei.val)
}

infillCritAKG = function(points, model, ctrl=NULL) {
  if(is.null(ctrl$new.noise.var)) ctrl$new.noise.var=0
  apply(des, 1, AEI, model=model$learner.model, new.noise.var=ctrl$new.noise.var)
}

infillCritAEIold = function(points, model, ctrl=NULL) {
  if(is.null(ctrl$new.noise.var)) ctrl$new.noise.var=0
  if(is.null(ctrl$y.min)) ctrl$y.min=NULL
  apply(points, 1, AEI, model=model$learner.model, new.noise.var=ctrl$new.noise.var, y.min=ctrl$y.min)
}  
  