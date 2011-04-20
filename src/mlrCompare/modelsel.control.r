#' @include OptControl.R
roxygen()

#' @exportClass modelsel.control
#' @rdname modelsel.control 

setClass(
  "modelsel.control",
  contains = c("OptControl")
)

#' Control structure for model selection from a finite amount of learners. 
#' 
#' @param minimize [logical] \cr 
#'       Minimize performance measure? Default is TRUE. 
#' @param path [\code{logical(1)}]\cr
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
    new("modelsel.control", minimize=minimize, path=path,  start=list())
  }
)
