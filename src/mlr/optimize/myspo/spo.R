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
  rep.pids = getRepeatedParameterIDs(par.set, with.nr=TRUE)
  opt.path = makeOptPath(y.names=control@y.name, x.names=rep.pids, minimize=control@minimize)
  if (length(opt.path@y.names) > 1)
    stop("'opt.path' should only contain one 'y' column!")
  y.name = opt.path@y.names
  
  if (is.null(des)) {
    des.x = makeDesign(control@init.design.points, par.set, control@init.design.fun, control@init.design.args)
    xs = lapply(1:nrow(des.x), function(i) designToList(des.x, par.set, i))
    ys = sapply(xs, fun)
    des = des.x
    des[, y.name] = ys
  } else {
    if (!(y.name %in% colnames(des)))
      stop("Design 'des' must contain y column of fitness values: ", y.name)
    ys = des[, y.name]
    # remove y 
    des.x = des[, -which(colnames(des) == y.name)]
    cns = colnames(des.x)
    if(!setequal(cns, rep.pids))
      stop("Column names of design 'des' must match names of parameters in 'par.set'!")
    # reorder
    des.x = des.x[, rep.pids]
    xs = lapply(1:nrow(des.x), function(i) designToList(des.x, par.set, i))
  }
  lapply(1:nrow(des.x), function(i) addPathElement(opt.path, x=as.list(des.x[i,]), y=ys[i]))
  rt = makeRegrTask(target=y.name, data=des)
  model = train(learner, rt)
  loop = 1
  res.vals = list()
  while(loop <= control@seq.loops) {
    if (loop %in% control@resample.at) {
      r = resample(learner, rt, control@ResampleDesc, measures=control@resample.measures)
      res.vals[[length(res.vals)+1]] = r$aggr
    }
    prop.des = proposePoints(model, par.set, control)
    xs = lapply(1:nrow(prop.des), function(i) designToList(prop.des, par.set, i))
    ys = sapply(xs, fun)
    lapply(1:nrow(prop.des), function(i) addPathElement(opt.path, x=as.list(prop.des[i,]), y=ys[i]))
    rt = makeRegrTask(target=y.name, data = as.data.frame(opt.path), exclude=c(".dob", ".eol"))
    model = train(learner, rt)
    loop = loop + 1  
  }
  e = getBestElement(opt.path, y.name)
  list(x=e$x, y=e$y, path=opt.path, model=model)
}


designToList = function(des, par.set, i, y.name) {
  des = des[i,]
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
    else if (p@type == "integervector") 
      x[[p@id]] = as.integer(des[,col])
    else 
      x[[p@id]] = des[,col]
  }
  return(x)
}






