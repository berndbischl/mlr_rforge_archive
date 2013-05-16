#FIXME: minimize
#FIXME: retrain kriging faster
#FIXME: handle error in meta learner
#FIXME: i think resample at and save.model at count differently
#FIXME: configure so we dont see learner output on default
#       maybe we do need a better way to configure mlr learners, possibyl by control?  

#FIXME: how to choose best element. with noise? without?
#FIXME: no more target function evals of the final point on default
#FIXME: different name for final evals in output (not last step number)
#FIXME: default for final point should be best point, not last step (especially when final evals are made)

#FIXME: cmaes doesn't work when optimum in constraints
#FIXME: separate infill criterion from optimzer

#'  Optimizes a function with sequential model based optimization.
#'
#' @param fun [\code{function(x, ...)}]\cr 
#'   Fitness function to minimize. The first argument has to be a list of values. 
#'   The function has to return a single numerical value.
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Collection of parameters and their constraints for optimization.   
#' @param design [\code{data.frame} | NULL]\cr
#'   Initial design. Must have been created by \code{\link[ParamHelpers]{generateDesign}}. 
#'   If the parameters have corresponding trafo functions, 
#'   the design must not be transformed before it is passed! 
#'   If \code{NULL}, one is constructed from the settings in \code{control}.
#' @param learner [\code{\link[mlr]{Learner}}]\cr
#'   Regression learner to model \code{fun}.  
#' @param control [\code{\link{MBOControl}}]\cr
#'   Control object for mbo.  
#' @param show.info [\code{logical(1)}]\cr
#'   Show info message after each function evaluation?
#'   Default is \code{TRUE}.
#' @param ... [any]\cr
#'   Further arguments passed to fitness function.
#' @return [\code{list}]:
#'   \item{x [\code{list}]}{Named list of proposed optimal parameters.}
#'   \item{y [\code{numeric(1)}]}{Value of fitness function at \code{x}, either from evals during optimization or from requested final evaluations, if those were greater than 0.}
#'   \item{opt.path [\code{\link[ParamHelpers]{OptPath}}]}{Optimization path.}
#'   \item{models [List of \code{\link[mlr]{WrappedModel}}]}{List of saved regression models.}
#' @export 

mbo = function(fun, par.set, design=NULL, learner, control, show.info=TRUE, ...) {
  if(any(sapply(par.set$pars, function(x) inherits(x, "LearnerParam"))))
    stop("No par.set parameter in 'mbo' can be of class 'LearnerParam'! Use basic parameters instead to describe you region of interest!")
  if (any(is.infinite(c(getLower(par.set), getUpper(par.set)))))
    stop("mbo requires finite box constraints!")
  if (control$infill.opt == "CMAES") 
    requirePackages("cmaes", "proposePoints")
  if (control$infill.opt == "CMAES" && control$propose.points != 1)
    stop("CMAES can only propose 1 point!")        
  if (control$infill.opt == "CMAES" &&
    !all(sapply(par.set$pars, function(p) p$type) %in% c("numeric", "integer", "numericvector", "integervector")))
    stop("Proposal method CMAES can only be applied to numeric, integer, numericvector, integervector parameters!")
  if (control$infill.opt == "EI" && 
    !(class(learner) %in% c("regr.km", "regr.kmforrester"))) 
    stop("Expected improvement can currently only be used with learner 'regr.km' and 'regr.kmforrester'!")
  if (learner$type != "regr")
    stop("mbo requires regression learner!")          
  if (control$infill.opt == "EI")
    requirePackages("DiceOptim")
  if (max(control$save.model.at) > control$seq.loops)
    stopf("Cannot save model at loop %i when just %i sequential.loops!", max(control$save.model.at), control$seq.loops)  
  
  # FIXME: doc and better control
  # save currently set options
  oldopts = list(
    ole = getOption("mlr.on.learner.error"),
    slo = getOption("mlr.show.learner.output")
  )
  
  # configure mlr in an appropriate way
  configureMlr(on.learner.error="warn", show.learner.output=FALSE)
  
  # get parameter ids repeated length-times and appended number
  rep.pids = getParamIds(par.set, repeated=TRUE, with.nr=TRUE)
  y.name = control$y.name
  opt.path = makeOptPathDF(par.set, y.name, control$minimize)
  
  if (is.null(design)) {
    design.x = generateDesign(control$init.design.points, par.set, 
      control$init.design.fun, control$init.design.args, trafo=FALSE)
    xs = lapply(1:nrow(design.x), function(i) ParamHelpers:::dfRowToList(design.x, par.set, i))
    ys = evalTargetFun(fun, par.set, xs, opt.path, control, show.info, oldopts, ...)
    design = design.x
    design[, y.name] = ys
  } else {
    if (attr(design, "trafo"))
      stop("Design must not be tranformed before call to 'mbo'. Set 'trafo' to FALSE in generateDesign.")
    if (!(y.name %in% colnames(design)))
      stop("Design 'design' must contain y column of fitness values: ", y.name)
    ys = design[, y.name]
    # remove y 
    design.x = design[, -which(colnames(design) == y.name), drop=FALSE]
    cns = colnames(design.x)
    if(!setequal(cns, rep.pids))
      stop("Column names of design 'design' must match names of parameters in 'par.set'!")
    # reorder
    design.x = design.x[, rep.pids, drop=FALSE]
    xs = lapply(1:nrow(design.x), function(i) ParamHelpers:::dfRowToList(design.x, par.set, i))
  }
  Map(function(x,y) addOptPathEl(opt.path, x=x, y=y, dob=0), xs, ys)
  rt = makeMBOTask(design, y.name, control=control)
  model = train(learner, rt)
  models = list()
  if (0 %in% control$save.model.at)
    models[[length(models)+1]] = model
  loop = 1
  res.vals = list()
  while(loop <= control$seq.loops) {
    if (loop %in% control$resample.at) {
      r = resample(learner, rt, control$resample.desc, measures=control$resample.measures)
      res.vals[[length(res.vals)+1]] = r$aggr
    }
    
    prop.design = proposePoints(model, par.set, control, opt.path)
    xs = lapply(1:nrow(prop.design), function(i) ParamHelpers:::dfRowToList(prop.design, par.set, i))
    xs = lapply(xs, repairPoint, par.set=par.set)
    ys = evalTargetFun(fun, par.set, xs, opt.path, control, show.info, oldopts, ...)
    
    Map(function(x,y) addOptPathEl(opt.path, x=x, y=y, dob=loop), xs, ys)
    rt = makeMBOTask(as.data.frame(opt.path, discretes.as.factor=TRUE), y.name, control=control)
    model = train(learner, rt)
    if (loop %in% control$save.model.at)
      models[[length(models)+1]] = model
    loop = loop + 1
  }
  names(models) = control$save.model.at
  names(res.vals) = control$resample.at
    
  design = getTaskData(rt, target.extra=TRUE)$data
  final.index = chooseFinalPoint(fun, par.set, model, opt.path, y.name, control)
  
  if (control$final.evals > 0) {
    prop.design = design[rep(final.index, control$final.evals),,drop=FALSE]
    xs = lapply(1:nrow(prop.design), function(i) ParamHelpers:::dfRowToList(prop.design, par.set, i))
    ys = evalTargetFun(fun, par.set, xs, opt.path, control, show.info, oldopts, ...)
    y = mean(ys)
    x = xs[[1]]
  } else {
    y = getOptPathEl(opt.path, final.index)$y
    x = ParamHelpers:::dfRowToList(design, par.set, final.index)
  }
  # restore mlr configuration
  configureMlr(on.learner.error=oldopts[["ole"]], show.learner.output=oldopts[["slo"]])
  # make sure to strip name of y
  structure(list(
    x=x,
    y=as.numeric(y),
    opt.path=opt.path,
    resample=res.vals,
    models=models
  ), class="MBOResult")
}

# Print mbo result object.
# 
# @param x [\code{\link{MBOResult}}]\cr
#   mbo result object instance.
# @param ... [any]\cr
#   Not used.
#' @S3method print MBOResult
print.MBOResult = function(x, ...) {
  op = x$opt.path
  catf("Recommended parameters:")
  catf(paramValueToString(op$par.set, x$x))
  catf("Objective: %s = %.3f\n", op$y.names[1], x$y)
  catf("Optimization path")
  catf("%i + %i entries in total, displaying last 10:",
    sum(op$env$dob == 0), length(op$env$dob))
  print(tail(as.data.frame(x$op), 10))
  
}