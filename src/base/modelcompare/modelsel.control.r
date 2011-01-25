#' @include control.opt.r
roxygen()

#' @exportClass modelsel.control
#' @rdname modelsel.control 

setClass(
  "modelsel.control",
  contains = c("opt.control")
)

#' Control structure for model selection from a finite amount of learners. 
#' 
#' @param minimize [logical] \cr 
#'       Minimize performance measure? Default is TRUE. 
#' @param path [boolean]\cr
#'        Should optimization path be saved?
#' 		    
#' @return Control structure for tuning.
#' @exportMethod modelsel.control
#' @rdname modelsel.control 
#' @title Control for modelsel search tuning. 


setGeneric(
  name = "modelsel.control",
  def = function(path) {
    if (missing(path))
      path=FALSE
    standardGeneric("modelsel.control")
  }
)


#' @rdname modelsel.control 

setMethod(
  f = "modelsel.control",
  signature = signature(minimize="logical", path="logical"),
  def = function(minimize, path) {
    pds = list()
    for (i in 1:length(ranges)) {
      pd = discrete.learner.parameter(id=names(ranges)[i], vals=as.list(ranges[[i]]))
      pds[[i]] = pd 
    }
    new("modelsel.control", minimize=minimize, path=path,
      start=list(), par.descs=pds)
  }
)
