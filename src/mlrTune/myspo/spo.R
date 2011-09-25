#todo: minimize
#todo: how to choose best element. with noise? without?
#todo: retrain kriging faster
#todo: handle error in meta learner
#todo: i think resample at and save.model at count differently

#'  Optimizes a function with sequential parameter optimization.
#'
#' @param fun [function(x, ...)]\cr 
#'   Fitness function to minimize. The first argument has to be a list of values. The function has to return a single numerical value.
#' @param par.set [\code{\linkS4class{ParameterSet}}] \cr
#'   Collection of parameters and their constraints for optimization.   
#' @param des [data.frame | NULL] \cr
#'   Initial design. Must have been created by \code{\link{makeDesign}}. 
#'   If the parameters have corresponding trafo functions, 
#'   the design must not be transformed before it is passed! 
#'   If \code{NULL}, one is constructed from the settings in \code{control}.
#' @param learner [\code{\linkS4class{Learner}}] \cr
#'   Regression learner to model \code{fun}.  
#' @param control [\code{\linkS4class{SPOControl}}] \cr
#'   Control object for SPO.  
#' @return A list with the following entries:
#' \describe{
#'   \item{x [named list]}{List of proposed optimal parameters.}
#'   \item{y [numeric]}{Value of fitness function at \code{x}, either form evals during optimization or from requested final evaluations, if they were gretater than 0.}
#'   \item{path [\code{\linkS4class{OptPath}}]}{Optimization path.}
#'   \item{models [List of \code{\linkS4class{WrappedModel}}]}{List of saved regression models.}
#' }
#' @export 

#todo: check learner is regression
spo = function(fun, par.set, des=NULL, learner, control) {
  if(any(sapply(par.set@pars, function(x) is(x, "LearnerParameter"))))
    stop("No par.set parameter in 'spo' can be of class 'LearnerParameter'! Use basic parameters instead to describe you region of interest!")
  if (any(is.infinite(c(lower(par.set), upper(par.set)))))
    stop("SPO requires finite box constraints!")
  if (control@propose.points.method == "CMAES") 
    require.packs("cmaes", "proposePoints")
  if (control@propose.points.method == "CMAES" && control@propose.points != 1)
    stop("CMAES can only propose 1 point!")        
  if (control@propose.points.method == "CMAES" &&
    !all(sapply(par.set@pars, function(p) p@type) %in% c("numeric", "integer", "numericvector", "integervector")))
    stop("Proposal method CMAES can only be applied to numeric, integer, numericvector, integervector parameters!")
  if (control@propose.points.method == "EI" && 
    !(class(learner) %in% c("regr.km", "regr.kmforrester"))) 
    stop("Expected improvement can currently only be used with learner 'regr.km' and 'regr.kmforrester'!")        
  if (control@propose.points.method == "EI")
    require.packs("DiceOptim", "spo")
  
  rep.pids = getRepeatedParameterIDs(par.set, with.nr=TRUE)
  y.name = control@y.name
  opt.path = new("OptPathDF", par.set=par.set, y.names=y.name, minimize=control@minimize)
  
  if (is.null(des)) {
    des.x = makeDesign(control@init.design.points, par.set, 
      control@init.design.fun, control@init.design.args, trafo=FALSE)
    xs = lapply(1:nrow(des.x), function(i) dataFrameRowToList(des.x, par.set, i))
    ys = evalTargetFun(fun, par.set, xs)
    des = des.x
    des[, y.name] = ys
  } else {
    if (attr(des, "trafo"))
      stop("Design must not be tranformed before call to 'spo'. Set 'trafo' to FALSE in makeDesign.")
    if (!(y.name %in% colnames(des)))
      stop("Design 'des' must contain y column of fitness values: ", y.name)
    ys = des[, y.name]
    # remove y 
    des.x = des[, -which(colnames(des) == y.name), drop=FALSE]
    cns = colnames(des.x)
    if(!setequal(cns, rep.pids))
      stop("Column names of design 'des' must match names of parameters in 'par.set'!")
    # reorder
    des.x = des.x[, rep.pids, drop=FALSE]
    xs = lapply(1:nrow(des.x), function(i) dataFrameRowToList(des.x, par.set, i))
  }
  Map(function(x,y) addPathElement(opt.path, x=x, y=y), xs, ys)
  rt = makeSpoTask(des, y.name, control=control)
  model = train(learner, rt)
  models = list()
  if (0 %in% control@save.model.at)
    models[[length(models)+1]] = model
  loop = 1
  res.vals = list()
  while(loop <= control@seq.loops) {
    if (loop %in% control@resample.at) {
      r = resample(learner, rt, control@resample.desc, measures=control@resample.measures)
      res.vals[[length(res.vals)+1]] = r$aggr
    }
    prop.des = proposePoints(model, par.set, control, opt.path)
    xs = lapply(1:nrow(prop.des), function(i) dataFrameRowToList(prop.des, par.set, i))
    ys = evalTargetFun(fun, par.set, xs)
    Map(function(x,y) addPathElement(opt.path, x=x, y=y), xs, ys)
    rt = makeSpoTask(as.data.frame(opt.path), y.name, exclude=c("dob", "eol"), control=control)
    model = train(learner, rt)
    if (loop %in% control@save.model.at)
      models[[length(models)+1]] = model
    loop = loop + 1
  }
  names(models) =  control@save.model.at
  names(res.vals) =  control@resample.at
  
  des = getData(rt, target.extra=TRUE)$data
  final.index = chooseFinalPoint(fun, par.set, model, opt.path, y.name, control)
  
  if (control@final.evals > 0) {
    prop.des = des[rep(final.index, control@final.evals),,drop=FALSE]
    xs = lapply(1:nrow(prop.des), function(i) dataFrameRowToList(prop.des, par.set, i))
    ys = evalTargetFun(fun, par.set, xs)
    y = mean(ys)
    x = xs[[1]]
  } else {
    y = getPathElement(opt.path, final.index)$y
    x = dataFrameRowToList(des, par.set, final.index)
  }
  
  list(x=x, y=y, path=opt.path, resample=res.vals, models=models)
}


evalTargetFun = function(fun, par.set, xs) {
  xs = lapply(xs, trafoVal, par=par.set)
  sapply(xs, fun)  
}

makeSpoTask = function(des, y.name, exclude=character(0), control) {
  if (any(sapply(des, is.integer)))
    des = as.data.frame(lapply(des, function(x) if(is.integer(x)) as.numeric(x) else x))
  if (control@rank.trafo)
    des[,y.name] = rank(des[,y.name])
  makeRegrTask(target=y.name, data=des, exclude=exclude)
}
