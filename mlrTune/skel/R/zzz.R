#' @import BBmisc
#' @import ParamHelpers
#' @import parallelMap
NULL

.onAttach <- function(libname, pkgname) {
  parallelRegisterLevels(package="mlrTune", levels="mlrTune.tune")
}