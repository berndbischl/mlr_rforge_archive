#' @include object.r
roxygen()



setClass(
  "Aggregation",
  contains = c("object"),
  representation = representation(
    id = "character",
    fun = "function"
  )
)


