#' @import BBmisc
#' @import ParamHelpers
#' @import boot
#' @import reshape
#' @importFrom stats predict
#' @importFrom codetools findGlobals

.onAttach <- function(libname, pkgname) {
  configureMlr()
}