#todo: minimize
#'  Optimizes a function with sequential parameter optimization.
#'
#' @param fun [function(x, ...)]\cr 
#'   Fitness function to minimize. The first argument has to be a list of values. The function has to return a single numerical value.
#' @param par.set [\code{\linkS4class{ParameterSet}}] \cr
#'   Collection of parameters and their constraints for optimization.   
#' @param des [data.frame | NULL] \cr
#'   Initial design. If \code{NULL}, one is constructed from the settings in \code{control}.
#' @param learner [\code{\linkS4class{Learner}}] \cr
#'   Regression learner to model \code{fun}.  
#' @param control [\code{\linkS4class{SPOControl}}] \cr
#'   Control object for SPO.  
#' @return The control object.  
#' @export 

#todo: check learner is regression
spo = function(fun, par.set, des=NULL, learner, control) {
  if (control@propose.points.method == "EI" && !is(learner, "regr.km")) 
    stop("Expected improvement can currently only be used with learner 'regr.km'!")        
  if(any(sapply(par.set@pars, function(x) is(x, "LearnerParameter"))))
    stop("No par.set parameter in 'spo' can be of class 'LearnerParameter'! Use basic parameters instead to describe you region of interest!")
  if (any(is.infinite(c(lower(par.set), upper(par.set)))))
    stop("SPO requires finite box constraints!")
  opt.path = makeOptPath(y.names=control@y.name, x.names=names(par.set@pars), minimize=control@minimize)
  if (length(opt.path@y.names) > 1)
    stop("'opt.path' should only contain one 'y' column!")
  y.name = opt.path@y.names
  
  if (is.null(des)) {
    des = makeDesign(control@init.design.points, par.set, control@init.design.fun, control@init.design.args)
    des$y = sapply(1:nrow(des), function(i) fun(as.list(des[i,])))
  }
  
  if (!(y.name %in% colnames(des)))
    stop("Design 'des' must contain y column of fitness values: ", y.name)
  for (i in 1:nrow(des))
    addPathElement(opt.path, x=as.list(des[i,colnames(des)!=y.name]), y=des[i,y.name])
  rt = makeRegrTask(target=y.name, data=des)
  model = train(learner, rt)
  loop = 1
  res.vals = list()
  while(loop <= control@seq.loops) {
    if (loop %in% control@resample.at) {
      r = resample(learner, rt, control@ResampleDesc, measures=control@resample.measures)
      res.vals[[length(res.vals)+1]] = r$aggr
    }
    xs = proposePoints(model, par.set, control)
    y = sapply(xs, fun)
    Map(function(x, y1) addPathElement(opt.path, x=x, y=y1), xs, y)
    rt = makeRegrTask(target=y.name, data = as.data.frame(opt.path), exclude=c(".dob", ".eol"))
    model = train(learner, rt)
    loop = loop + 1  
  }
  e = getBestElement(opt.path, y.name)
  list(x=e$x, y=e$y, path=opt.path, model=model)
}






