#FIXME check whether model (and settings), ctriteria, optimizer, noisy 
# work together
# check whether the user selected valid options / combinations
checkStuff = function(fun, par.set, design, learner, control) {
  checkArg(fun, "function")
  # FIXME: we wil probably remove learnerparams anyeway
  if(any(sapply(par.set$pars, function(x) inherits(x, "LearnerParam"))))
    stop("No par.set parameter in 'mbo' can be of class 'LearnerParam'! Use basic parameters instead to describe you region of interest!")
  if (any(is.infinite(c(getLower(par.set), getUpper(par.set)))))
    stop("mbo requires finite box constraints!")
  if (control$propose.points > 1L) 
    stop("Currently only 1 point can be proposed per iteration!")        
  if (control$infill.opt == "cmaes" &&
      !all(sapply(par.set$pars, function(p) p$type) %in% c("numeric", "integer", "numericvector", "integervector")))
    stop("Optimizer CMAES can only be applied to numeric, integer, numericvector, integervector parameters!")
  if (learner$type != "regr")
    stop("mbo requires regression learner!")   
  if (control$infill.crit %in% c("ei", "aei") && learner$predict.type != "se")
    stopf("For infill criterion '%s' predict.type of learner %s must be set to 'se'!%s",
      control$infill.crit, learner$id, 
      ifelse(learner$se, "", "\nBut this learner does not seem to support prediction of standard errors!"))
    # for EI we need mu + sd     
  #  if (control$infill.opt == "EI" && 
  #          !(class(learner) %in% c("regr.km", "regr.kmforrester"))) 
  #      stop("Expected improvement can currently only be used with learner 'regr.km' and 'regr.kmforrester'!")
  if (max(control$save.model.at) > control$seq.loops)
    stopf("Cannot save model at loop %i when just %i sequential.loops!", 
      max(control$save.model.at), control$seq.loops)  
}