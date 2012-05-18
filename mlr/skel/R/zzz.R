.onLoad <- function(libname, pkgname) {
  #FIXME defaults
  configureMlr(on.learner.error="stop")
}