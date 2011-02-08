#' @export
makeSPOFunction = function(fun) {
  force(fun)
  function(x, ...) {
    fun(unlist(x), ...)
  }
}
