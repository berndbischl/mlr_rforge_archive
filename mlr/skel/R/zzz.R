#' @import BBmisc
#' @import ParamHelpers
#' @import boot
#' @importFrom stats predict
#' @importFrom codetools findGlobals

.onAttach <- function(libname, pkgname) {
  #FIXME defaults
  configureMlr(on.learner.error="stop")
}