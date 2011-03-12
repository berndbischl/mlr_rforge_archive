#todo: minimize
#todo: how to choose best element. with noise? without?
#todo: retrain kriging faster
#'  Optimizes a function with sequential parameter optimization.
#'
#' @param fun [function(x, ...)]\cr 
#'   Fitness function to minimize. The first argument has to be a list of values. The function has to return a single numerical value.
#' @param par.set [\code{\linkS4class{ParameterSet}}] \cr
#'   Collection of parameters and their constraints for optimization.   
#' @param des [data.frame | NULL] \cr
#'   Initial design. Must have been created by \code{\link{makeDesign}}. 
#'   If the parameters have correspinding trafo functions, 
#'   the design must not be transformed before it is passed! 
#'   If \code{NULL}, one is constructed from the settings in \code{control}.
#' @param learner [\code{\linkS4class{Learner}}] \cr
#'   Regression learner to model \code{fun}.  
#' @param control [\code{\linkS4class{SPOControl}}] \cr
#'   Control object for SPO.  
#' @return The control object.  
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
  opt.path = makeOptPath(y.names=y.name, x.names=names(par.set@pars), minimize=control@minimize)
  
  if (is.null(des)) {
    des.x = makeDesign(control@init.design.points, par.set, 
      control@init.design.fun, control@init.design.args, trafo=FALSE)
    xs = lapply(1:nrow(des.x), function(i) designToList(des.x, par.set, i))
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
    xs = lapply(1:nrow(des.x), function(i) designToList(des.x, par.set, i))
  }
  Map(function(x,y) addPathElement(opt.path, x=x, y=y), xs, ys)
  rt = makeSpoTask(des, y.name)
  model = train(learner, rt)
  models = list()
  if (0 %in% control@save.model.at)
    models[[length(models)+1]] = model
  loop = 1
  res.vals = list()
  while(loop <= control@seq.loops) {
    if (loop %in% control@resample.at) {
      r = resample(learner, rt, control@ResampleDesc, measures=control@resample.measures)
      res.vals[[length(res.vals)+1]] = r$aggr
    }
    prop.des = proposePoints(model, par.set, control, opt.path)
    xs = lapply(1:nrow(prop.des), function(i) designToList(prop.des, par.set, i))
    ys = evalTargetFun(fun, par.set, xs)
    Map(function(x,y) addPathElement(opt.path, x=x, y=y), xs, ys)
    rt = makeSpoTask(as.data.frame(opt.path), y.name, exclude=c("dob", "eol"))
    model = train(learner, rt)
    if (loop %in% control@save.model.at)
      models[[length(models)+1]] = model
    loop = loop + 1  
  }
  e = getBestElement(opt.path, y.name)
  names(models) =  control@save.model.at
  list(x=e$x, y=e$y, path=opt.path, models=models)
}


designToList = function(des, par.set, i, y.name) {
  des = des[i,,drop=FALSE]
  pars = par.set@pars
  col = 0
  x = list()
  for (i in 1:length(pars)) {
    p = pars[[i]]
    cc = rev(col)[1]
    if (p@type %in% c("numericvector", "integervector")) 
      col = (cc + 1) : (cc + length(lower(p)))   
    else 
      col = cc + 1    
    
    if (p@type == "numericvector") 
      x[[p@id]] = as.numeric(des[,col])  
    else if (p@type %in% c("integer", "integervector")) 
      x[[p@id]] = as.integer(round(des[,col]))
    else if (p@type == "discrete") 
      x[[p@id]] = as.character(des[,col])
    else 
      x[[p@id]] = des[,col]
  }
  return(x)
}

evalTargetFun = function(fun, par.set, xs) {
  xs = lapply(xs, trafoVal, par=par.set)
  sapply(xs, fun)  
}

makeSpoTask = function(des, y.name, exclude=character(0)) {
  if (any(sapply(des, is.integer)))
    des = as.data.frame(lapply(des, function(x) if(is.integer(x)) as.numeric(x) else x))
  makeRegrTask(target=y.name, data=des, exclude=exclude)
}
