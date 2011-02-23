#' @include object.r
roxygen()

#todo: document better
#' A aggregation method reduce the performance values of the test (and possibly the training sets) to a single
#' value. 
#' @exportClass Aggregation

setClass(
  "Aggregation",
  contains = c("object"),
  representation = representation(
    id = "character",
    fun = "function"
  )
)


