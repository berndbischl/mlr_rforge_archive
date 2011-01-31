#' @export
makeSPOFunction = function(fun) {
  function(x, ...) {
    f(unlist(x), ...)
  }
}
