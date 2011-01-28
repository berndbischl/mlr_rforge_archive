#' @include control.varsel.r
roxygen()

#' @exportClass exhvarsel.control
#' @rdname exhvarsel.control 

setClass(
  "exhvarsel.control",
  contains = c("varsel.control")
)

#' Constructor.
setMethod(
    f = "initialize",
    signature = signature("exhvarsel.control"),
    def = function(.Object, path, same.resampling.instance, max.vars) {
      callNextMethod(.Object, path=path, max.vars=max.vars, maxit=.Machine$integer.max)
    }
)


#' Control structure for exhaustive variable selection. 
#' 
#' @param path [boolean]\cr
#'   Should optimization path be saved? Default is TRUE.
#' @param same.resampling.instance [logical] \cr
#'    Should the same resampling instance be used for all evaluations to reduce variance? Default is \code{TRUE}.
#' @param max.vars [integer] \cr 
#'   Maximal number of allowed variables searched sets. Default is max. integer.
#'        
#' @return Control structure.
#' @exportMethod exhvarsel.control
#' @rdname exhvarsel.control 
#' @title Control structure for exhaustive variable selection. 


setGeneric(
  name = "exhvarsel.control",
  def = function(path, same.resampling.instance, max.vars) {
    if (missing(path))
      path = TRUE
    if (missing(same.resampling.instance))
      same.resampling.instance = TRUE
    if (missing(max.vars))
      max.vars = .Machine$integer.max
    if (is.numeric(max.vars))
      max.vars = as.integer(max.vars)
    standardGeneric("exhvarsel.control")
  }
)

#' @rdname exhvarsel.control 

setMethod(
  f = "exhvarsel.control",
  signature = signature(path="logical", same.resampling.instance="logical", max.vars="integer"),
  def = function(path, same.resampling.instance, max.vars) {
    new("exhvarsel.control", path=path, same.resampling.instance=same.resampling.instance, max.vars=max.vars)
  }
)



