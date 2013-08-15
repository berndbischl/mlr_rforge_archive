#' @import BBmisc
#' @import parallelMap
#' @import ParamHelpers
#' @import boot
#' @importFrom stats predict
#' @importFrom codetools findGlobals

.onAttach <- function(libname, pkgname) {
  configureMlr()
}