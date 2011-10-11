#' @importClassesFrom mlr BaseWrapper WrappedModel
#' @importMethodsFrom mlr makeWrappedModel
roxygen <- function() NULL

#' @importFrom utils packageDescription
.onLoad <- function(libname, pkgname) {
  packageStartupMessage("Loading package mlrChains. Version: ", packageDescription("mlrChains", fields="Version"))
}