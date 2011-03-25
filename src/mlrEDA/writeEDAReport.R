
report = function(outdir, data, target, exclude=character(0), control) {
#  assign(".sweave.data", data, envir=.GlobalEnv)
#  assign(".sweave.target", target, envir=.GlobalEnv)
#  assign(".sweave.exclude", exclude, envir=.GlobalEnv)
#  assign(".sweave.control", control, envir=.GlobalEnv)
#  Sweave("d:/sync/projekte/mlr/src/mlrEDA/writeEDAReport.Rnw")
#  rm(".sweave.data", envir=.GlobalEnv)
#  rm(".sweave.target", envir=.GlobalEnv)
#  rm(".sweave.exclude", envir=.GlobalEnv)
#  rm(".sweave.control", envir=.GlobalEnv)
  dir = getwd()
  setwd(outdir)
  e = new.env()
  e$data = data
  e$target = target
  brew("d:/sync/projekte/mlr/src/mlrEDA/writeEDAReport_html.brew", out="c:/brew/bla.html", envir=e) 
  setwd(dir)
}


report("c:/brew", Glass, "Type")
