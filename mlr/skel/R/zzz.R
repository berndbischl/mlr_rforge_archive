#' @import BBmisc
#' @import ParamHelpers
#' @import boot

.onAttach <- function(libname, pkgname) {
  #FIXME defaults
  configureMlr(on.learner.error="stop")
}