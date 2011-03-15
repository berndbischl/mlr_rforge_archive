
report = function(outdir, data, target, exclude=character(0)) {
  assign(".sweave.data", data, envir=.GlobalEnv)
  assign(".sweave.target", target, envir=.GlobalEnv)
  assign(".sweave.exclude", exclude, envir=.GlobalEnv)
  dir = getwd()
  setwd(outdir)
  Sweave("d:/sync/projekte/mlr/src/mlrEDA/writeEDAReport.Rnw")
  rm(".sweave.data", envir=.GlobalEnv)
  rm(".sweave.target", envir=.GlobalEnv)
  rm(".sweave.exclude", envir=.GlobalEnv)
  setwd(dir)
}

report("src/mlrEDA", iris, "Species")
