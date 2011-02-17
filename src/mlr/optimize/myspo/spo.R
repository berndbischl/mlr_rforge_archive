#todo: minimize
#'  Optimizes a function with sequential parameter optimization.
#'
#' @param fun [function(x, ...)]\cr 
#'   Fitness function to minimize. The first argument has to be a list of values. The function has to return a single numerical value.    
#' @param propose.points [integer(1)]\cr 
#'   Number of proposed points after optimizing the surrogate model with \code{propose.points.methods}.   
#' @param propose.points.method [character(1)]\cr 
#'   How should points be proposed by using the surrogate model. Possible are: 
#'   'seq.design': Use a large design of points and evaluate the surrogate model at each. The best \code{propose.points} are selected.    
#' @param seq.design.points [integer(1)]\cr 
#'   Number of points in sequential design. Only used if \code{propose.points.method} is 'seq.design.' Default is 10000.   
#' @param seq.design.points [integer(1)]\cr 
#'   Number of points in sequential design. Only used if \code{propose.points.method} is 'seq.design.' Default is 10000.   
#' @param seq.design.fun [function] \cr
#'   Function from package lhs for the sequentail design. Possible are: maximinLHS, randomLHS, geneticLHS, improvedLHS, , optAugmentLHS, optimumLHS.
#'   Only used if \code{propose.points.method} is 'seq.design.' Default is 'randomLHS'. 
#' @param seq.design.args [list] \cr
#'   List of further arguments passed to \code{seq.design.fun}.  
#'   Only used if \code{propose.points.method} is 'seq.design.' Default is empty list.
#' @param save.model.at [integer] \cr
#'   Vector of sequential optimzation iterations when the model should be saved. Iteration 1 is the model fit for the initial design.
#'   Default is \code{c(1, seq.loops)}.
#' @return The control object.  
#' @export 

spo = function(fun, par.set, des, learner, control) {
  if (control$propose.points.method == "EI" && !is(learner, "regr.km")) 
    stop("Expected improvement can currently only be used with learner 'regr.km'!")        
  if(any(sapply(par.set@pars, function(x) is(x, "LearnerParameter"))))
    stop("No par.set parameter in 'spo' can be of class 'LearnerParameter'! Use basic parameters instead to describe you region of interest!")
  opt.path = makeOptPath(y.names=control$y.name, x.names=names(par.set@pars), minimize=control$minimize)
  if (length(opt.path@y.names) > 1)
    stop("'opt.path' should only contain one 'y' column!")
  y.name = opt.path@y.names
  if (!(y.name %in% colnames(des)))
    stop("Design 'des' must contain y column of fitness values: ", y.name)
  for (i in 1:nrow(des))
    addPathElement(opt.path, x=as.list(des[i,colnames(des)!=y.name]), y=des[i,y.name])
  rt = makeRegrTask(target=y.name, data=des)
  model = train(learner, rt)
  loop = 1
  res.vals = list()
  while(loop <= control$seq.loops) {
    if (loop %in% control$resample.at) {
      r = resample(learner, rt, control$ResampleDesc, measures=control$resample.measures)
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






