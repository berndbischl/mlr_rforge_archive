#FIXME: minimize
#FIXME: how to choose best element. with noise? without?
#FIXME: retrain kriging faster
#FIXME: handle error in meta learner
#FIXME: i think resample at and save.model at count differently
#FIXME: allow .... and pass it on to fun (DONE)
#FIXME: add show.info
#FIXME: configure so we dont see learner output on default
#       maybe we do need a better way to configure mlr learners, possibyl by control?  
#FIXME: no more target function evals of the final point on default
#FIXME: different name for final evals in output (not last step number)
#FIXME: default for final point should be best point, not last step (especially when final evals are made)
#FIXME: cmaes doesn't work when optimum in constraints
#FIXME: result object with print method
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
#' @param ... [\coed{list}]\cr
#'   Further arguments passed to fitness function.
#' @return [\code{list}]:
#'   \item{x [\code{list}]}{Named list of proposed optimal parameters.}
#'   \item{y [\code{numeric(1)}]}{Value of fitness function at \code{x}, either from evals during optimization or from requested final evaluations, if those were greater than 0.}
#'   \item{path [\code{\link[ParamHelpers]{OptPath}}]}{Optimization path.}
#'   \item{models [List of \code{\link[mlr]{WrappedModel}}]}{List of saved regression models.}
#' @export 

mbo = function(fun, par.set, design=NULL, learner, control, show.info=TRUE, ...) {
  if(any(sapply(par.set$pars, function(x) inherits(x, "LearnerParam"))))
    stop("No par.set parameter in 'mbo' can be of class 'LearnerParam'! Use basic parameters instead to describe you region of interest!")
  if (any(is.infinite(c(getLower(par.set), getUpper(par.set)))))
    stop("mbo requires finite box constraints!")
  if (control$propose.points.method == "CMAES") 
    requirePackages("cmaes", "proposePoints")
  if (control$propose.points.method == "CMAES" && control$propose.points != 1)
    stop("CMAES can only propose 1 point!")        
  if (control$propose.points.method == "CMAES" &&
    !all(sapply(par.set$pars, function(p) p$type) %in% c("numeric", "integer", "numericvector", "integervector")))
    stop("Proposal method CMAES can only be applied to numeric, integer, numericvector, integervector parameters!")
  if (control$propose.points.method == "EI" && 
    !(class(learner) %in% c("regr.km", "regr.kmforrester"))) 
    stop("Expected improvement can currently only be used with learner 'regr.km' and 'regr.kmforrester'!")
  if (learner$type != "regr")
    stop("mbo requires regression learner!")          
  if (control$propose.points.method == "EI")
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
  list(x=x, y=as.numeric(y), path=opt.path, resample=res.vals, models=models)
}

#' Evaluates target fitness function on given set of points.
#'
#' @param fun [\code{function(x, ...)}]\cr 
#'   Fitness function to minimize. The first argument has to be a list of values. 
#'   The function has to return a single numerical value.
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Collection of parameters and their constraints for optimization.
#' @param xs [\code{list}]\cr
#'   Set of points on which fun shall be evaluated.
#' @param opt.path [\code{\link[ParamHelpers]{OptPath}}]\cr
#'   Optimazation path to save of type \code{\link[ParamHelpers]{OptPath}}.
#' @param control [\code{\link{MBOControl}}]\cr
#'   Control object for mbo.  
#' @param show.info [\code{logical(1)}]\cr
#'   Show info message after each function evaluation?
#'   Default is \code{TRUE}.
#' @param oldopts [\code{list}]\cr
#'   Old mlr configuration.
#' @param ... [\code{list}]\cr
#'   Further arguments passed to fitness function.
#' @return [\code{list}]:
#'   \item{x [\code{list}]}{Named list of proposed optimal parameters.}
#'   \item{y [\code{numeric(1)}]}{Value of fitness function at \code{x}, either from evals during optimization or from requested final evaluations, if those were greater than 0.}
#'   \item{path [\code{\link[ParamHelpers]{OptPath}}]}{Optimization path.}
#'   \item{models [List of \code{\link[mlr]{WrappedModel}}]}{List of saved regression models.}
evalTargetFun = function(fun, par.set, xs, opt.path, control, show.info, oldopts, ...) {
  xs = lapply(xs, trafoValue, par=par.set)
  fun2 = function(x) {
    if (control$impute.errors) {
      y = try(fun(x, ...), silent=control$silent)
      if (is.error(y))
        y = as.numeric(NA)
    } else {
      y = fun(x, ...)
    }
    if(length(y) > 1) {
      stop("function output is not univariate!")
    }  
    if (show.info) {
      dob = opt.path$env$dob
      dob = if (length(dob) == 0) 0 else max(dob) + 1
      messagef("[mbo] %i: %s : %s=%.3f", dob, 
       paramValueToString(par.set, x), control$y.name, y)
    }
    return(y)
  }
  # restore mlr configuration
  configureMlr(on.learner.error=oldopts[["ole"]], show.learner.output=oldopts[["slo"]])
  ys = sapply(xs, fun2)  
  configureMlr(on.learner.error="warn", show.learner.output=FALSE)
  j = which(is.na(ys) | is.nan(ys) | is.infinite(ys))
  if (length(j) > 0) {
    ys[j] = mapply(control$impute, xs[j], ys[j], 
      MoreArgs=list(opt.path=opt.path), USE.NAMES=FALSE)
  }
  return(ys)
}

#' Generates MBO task.
#'
#' @param design [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Initial design.
#' @param y.name [\code{character(1)}]\cr
#'   Name of y-column for target values in optimization path. 
#' @return [\code{\link[mlr]{SupervisedTask}]:
#'   List of repaired points.
makeMBOTask = function(design, y.name, control) {
  design$dob = NULL
  design$eol = NULL
  if (any(sapply(design, is.integer)))
    design = as.data.frame(lapply(design, function(x) if(is.integer(x)) as.numeric(x) else x))
  #if (control$rank.trafo)
  #  design[,y.name] = rank(design[,y.name])
  makeRegrTask(target=y.name, data=design)
}

#' Repairs points below bound.
#'
#' (Sometimes we get one eps below bounds at least after EI)
#' 
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Collection of parameters and their constraints for optimization.
#' @param x [\code{list}]\cr
#'   List of values which evantually are located below bounds.
#' @return [\code{list}]:
#'   List of repaired points.
repairPoint = function(par.set, x) {
  Map(function(p, v) {
    if (p$type %in% c("numeric", "numericvector", "integer", "integervector")) {
      if (any(v < p$lower) | any(v > p$upper)) {
        warningf("Repairing value for %s: %s", p$id, as.character(v))
        v = pmax(p$lower, v)
        v = pmin(p$upper, v)
      }
    }
    return(v)
  }, par.set$pars, x)
}
