#' Extract data in task. Useful in \code{\link{trainLearner}} when you add a learning 
#' machine to the package.
#' 
#' @param task [\code{\linkS4class{LearnTask}}]\cr 
#'   Learning task.   
#' @param subset [\code{integer}] \cr 
#'   Selected cases. Default is all cases. 
#' @param vars [character] \cr 
#'   Selected inputs.  Default is all input variables.
#' @param target.extra [\code{logical(1)}] \cr 
#'   Should target vector be returned separately? 
#'   If not, a single data.frame including the target is returned, otherwise a list 
#'   with the input data.frame and an extra vector for the targets.
#'   Default is FALSE. 
#' @param class.as [\code{character(1)}] \cr
#'   Should target classes be recoded? Only for binary classification.
#'   Possible are \dQuote{factor} (do nothing), \dQuote{01}, and \dQuote{-1+1}. 
#'   In the two latter cases the target vector, which is usually a factor, is converted into a numeric vector. 
#'   The positive class is coded as +1 and the negative class either as 0 or -1. 
#'   Default is \dQuote{factor}.
#'    
#' @return Either a data.frame or a list with data.frame \code{data} and vector \code{target}.
#'
#' @export
#' @rdname getData
#' @title Extract data in task. 
getData = function(task, subset, vars, target.extra=FALSE, class.as="factor") {
  checkArg(class.as, choices=c("factor", "01", "-1+1"))
  
  # maybe recode y
  rec.y = function(y) {
    if (class.as=="01")
      as.numeric(y == task@desc@positive)
    else if (class.as=="-1+1")
      2*as.numeric(y == task@desc@positive)-1
    else
      y
  }
  
  tn = task@desc@target
  ms = missing(subset) || identical(subset, 1:task@desc@size)
  mv = missing(vars) || identical(vars, getFeatureNames(task))
  
  if (target.extra) {
    list(
      data = 
        if (ms && mv) 
        {d=task@dataenv$data;d[,tn]=NULL;d} 
      else if (ms)
        task@dataenv$data[,vars,drop=FALSE]
      else if (mv)
      {d=task@dataenv$data[subset,,drop=FALSE];d[,tn]=NULL;d} 
      else
        task@dataenv$data[subset,vars,drop=FALSE],
      target = 
        if (ms)
          rec.y(getTargets(task))
      else
        rec.y(getTargets(task)[subset])
      )
  } else {
    d = 
      if (ms && mv) 
        task@dataenv$data 
    else if (ms)
      task@dataenv$data[,c(vars, tn),drop=FALSE]
    else if (mv)
      task@dataenv$data[subset,,drop=FALSE]
    else
      task@dataenv$data[subset,vars,drop=FALSE]
    if (class.as != "factor")
      d[,tn] = rec.y(d[, tn])
    return(d)
  }
}
