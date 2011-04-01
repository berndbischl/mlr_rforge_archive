#' Write a HTML report which explores a given data set.
#'
#' @param out.dir [\code{character(1)}]\cr 
#'   Directory where files are written to. 
#' @param data [\code{data.frame}]\cr 
#'   Data. 
#' @param target [\code{character(1)}]\cr 
#'   Target column. 
#' @param name [\code{character(1)}]\cr 
#'   Name of dataset. Used as prefix for generated files 
#'   Default is name of R variable passed to \code{data}. 
#' @return None.
#' @export
#' @title Write EDA HTML report.

writeEDAReport = function(out.dir, data, target, name) {
  if (missing(name))
    name = deparse(substitute(data))
  dir = getwd()
  setwd(out.dir)
  e = new.env()
  e$name = name
  e$data = data
  e$target = target
  fn.out = file.path(out.dir, paste(name, "html", sep="."))
  fn.in = "d:/sync/projekte/mlr/src/mlrEDA/writeEDAReport_html.brew"
  #fn.in = system.file("writeEDAReport_html.brew", package="mlrEDA")
  print(fn.in)
  brew(fn.in, out=fn.out, envir=e) 
  setwd(dir)
}
data = Glass
target = "Type"
name = "Glass"
writeEDAReport("c:/brew", data, target, name)
