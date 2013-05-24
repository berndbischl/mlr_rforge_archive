# Infill criteria.
# CONVENTION: INFILL CRITERIA ARE ALWAYS MINIMIZED. SO A FEW BELOW ARE NEGATED VERSIONS!
#FIXME think about which criterias are for determinitic, which are for noisy case
# below is just guessed this...

# General interface
# 
# @param points [\code{data.frame}]\cr 
#   Points where to evaluate.
# @param model [\code{\link{WrappedModel}}]\cr
#   Model fitted on design.
# @param control [\code{\link{MBOControl}}]\cr
#   Control object.
# @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Parameter set.
# @param design [\code{data.frame}]\cr 
#   Design of already visited points.
# @return [\code{numeric}]. Criterion values at \code{points}.

# mean response of model
# useful for deterministic and noisy
infillCritMeanResponse = function(points, model, control, par.set, design) {
  predict(model, newdata=points)$data$response
}

# expected improvement
# useful for deterministic
infillCritEI = function(points, model, control, par.set, design) {
  y.min = min(design[, control$y.name])
  p = predict(model, newdata = points)$data
  xcr = (y.min - p$response) / p$se
  #FIXME: what is done in DiceOption::EI here for numerical reasons?
  #if (kriging.sd/sqrt(model@covariance@sd2) < 1e-06) {
  #  res = 0
  #  xcr = xcr.prob = xcr.dens = NULL
  #
  xcr.prob = pnorm(xcr)
  xcr.dens = dnorm(xcr)
  ei = (y.min -  p$response) * xcr.prob + p$se * xcr.dens
  return(-ei)
}

# augmented expected improvement, as designed by huang
# useful for noisy
infillCritAEI = function(points, model, control, par.set, design) {
  #FIXME: generalize new.noise.var for all models
  new.noise.var=model$learner.model@covariance@nugget
  pred = predict(model, newdata = design)$data
  qk = pred$response + qnorm(0.75) * pred$se
  y.min = pred$response[which.min(qk)]
  pred = predict(model, newdata = points)$data
  mk = pred$response
  sk = pred$se
  xcr = (y.min - mk)/sk
  xcr.prob = pnorm(xcr)
  xcr.dens = dnorm(xcr)
  #if (sk < sqrt(model@covariance@sd2)/1e+06) {
  #FIXME: What actually happens here. Find out in DiceOptim
  #FIXME: calculate aei.val as vector
  if (sk < 1e-06) {
    aei.val = 0
  } else {
    aei.val = ((y.min - mk) * xcr.prob + sk * xcr.dens) * 
      (1 - sqrt(new.noise.var)/sqrt(new.noise.var + sk^2))
  }
  return(aei.val)
}

# infillCritAKG = function(points, model, ctrl=NULL) {
#   if(is.null(ctrl$new.noise.var)) ctrl$new.noise.var=0
#   apply(des, 1, AEI, model=model$learner.model, new.noise.var=ctrl$new.noise.var)
# }
# 
# infillCritAEIold = function(points, model, ctrl=NULL) {
#   if(is.null(ctrl$new.noise.var)) ctrl$new.noise.var=0
#   if(is.null(ctrl$y.min)) ctrl$y.min=NULL
#   apply(points, 1, AEI, model=model$learner.model, new.noise.var=ctrl$new.noise.var, y.min=ctrl$y.min)
# }  
#   