# todo: errs for different and resampling measures? how should it look?
# todo: remove mlr:::
#' Tries to select multiple pairs of relevant features in order to produce 2D plots.
#' Features are selected by sequential forward search. After a pair is selected, either the 
#' best feature of the pair or both are removed from the task and the forward search is run again.   
#'
#' @param learner [\code{\linkS4class{learner}} or string]\cr 
#'   Learning algorithm. See \code{\link{learners}}.  
#' @param task [\code{\linkS4class{LearnTask}}] \cr
#'   Learning task.   
#' @param resampling [\code{\linkS4class{resample.instance}}] or [\code{\linkS4class{resample.desc}}]\cr
#'   Resampling strategy to evaluate feature sets. If you pass a description, 
#'   it is instantiated once at the beginning by default, so all feature sets are evaluated on the same training/test sets.
#'   If you want to change that behaviour, look at the control object.  
#' @param measures [list of \code{\linkS4class{measure}}]\cr
#'   Performance measures to evaluate. The first measure, aggregated by the first aggregation function is optimized during selection, others are simply evaluated.  
#' @param control [\code{\link{sequential.control}}]
#'   Control object for forward search. Its parameter \code{method} must be set to 'sfs'.  
#' @param pairs [integer(1)]
#'   Number of feature pairs to select. Default is 2.
#' @param remove [see \code{\link{sequential.control}}]
#'   How many features should be removed from the task after a pair is selected? 
#'   Set to 1 to remove only the best feature or set to 2 to remove both features of the selected pair.
#'   Default is 1.  
#' 
#' @return A list with the follwoing to elements: 'ors' is a list of length \code{pairs} with elements of 
#'   of class \code{\linkS4class{opt.result}}, and 'errs' is a data.frame ???????????? 
#' 
#' @export
#' @seealso \code{\link{makeVarselWrapper}} 
#' @title Variable selection.

varsel2d <- function(learner, task, resampling, measures, control, pairs, remove) {
  if (is.character(learner))
    learner <- make.learner(learner)
  if (is(resampling, "resample.desc") && control@same.resampling.instance)
    resampling = make.res.instance(resampling, task=task)
  if (missing(measures))
    measures = mlr:::default.measures(task)
  if (is(measures, "measure"))
    measures = list(measures)   
  if (!(is(control, "sequential.control") && control@method == "sfs"))
    stop("'control' must be sequential.control with method 'sfs'!")
  # todo: document this! really do this? maybe dont have the user pass the control...!
  control@alpha = -Inf
  control@max.vars = 2L  
  if (missing(pairs))
    pairs = 4L
  if (missing(remove))
    remove = 1L
  if (!(remove == 1 || remove == 2))
    stop("'remove' must be 1 or 2!")
  
  ors = list()
  errs = matrix(TRUE, nrow=task["size"], pairs)
  mode(errs) = "logical"
  
  for (i in 1:pairs) {
    bit.names = mlr:::getFeatureNames(task)
    if (length(bit.names) < 2)
      stop("Not enough features left to find another pair!")
    bits.to.features = function(x, task) binary.to.vars(x, bit.names)
    opt.path = makeOptimizationPathFromMeasures(bit.names, measures)
    ors[[i]] = varsel(learner, task, resampling, measures, bit.names, bits.to.features, control, opt.path)
    task2d = subset(task, vars=ors[[i]]@x)
    r = resample(learner, task2d, resampling)
    e = r$pred["truth"] != r$pred["response"]
    errs[,i] = e[order(r$pred["id"])]
    vars = setdiff(bit.names, ors[[i]]@x[1:remove])
    task = subset(task, vars=vars)
  } 
  return(list(ors=ors, errs=errs))
}

# todo: for regression cut thru y: high, medium, low
plot.varsel2d = function(task, ors, trafo.x=identity, trafo.y=identity) {
  df = data.frame()
  for (i in 1:length(ors)) {
    or = ors[[i]]
    f1 = or@x[1]
    f2 = or@x[2]
    v1 = fs[,f1]
    v2 = fs[,f2]
    v1 = trafo.x(v1)
    v2 = trafo.x(v2)
    df = rbind(df, data.frame(plot=i, v1=v1, v2=v2, y=y, errs=errs[,i]))
  }
  
  plot2d()
  
  
  df$plot = as.factor(df$plot)
  
  plt <- ggplot(df, aes(x=v1, y=v2, label=fun, colour=y))
  plt <- plt + geom_point(data=subset(df, wron), size=I(14), alpha=I(0.2), colour="black")
  plt <- plt + geom_text()
  plt <- plt + scale_x_continuous(name=f1)
  plt <- plt + scale_y_continuous(name=f1)
  plt
}
