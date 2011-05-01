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
#' @param brew.template [\code{character(1)} | NULL]\cr 
#'   File path to brew template to generate the report from.
#'   Default is NULL, which means the template from the package is used 
#'  (which is what you want).
#' @return None.
#' @export
#' @title Write EDA HTML report.

writeEDAReport = function(out.dir, data, target, name, brew.template=NULL) {
  require.packs("brew", "writeEDAReport")
  if (missing(name))
    name = deparse(substitute(data))
  dir = getwd()
  setwd(out.dir)
  e = new.env()
  e$name = name
  e$data = data
  e$target = target
  fn.out = file.path(out.dir, paste(name, "html", sep="."))
  if (is.null(brew.template) || brew.template == "")
    brew.template = system.file("writeEDAReport_html.brew", package="mlrEDA")
  message("Using brew template: ", brew.template)
  message("Writing output to: ", out.dir)
  brew(brew.template, out=fn.out, envir=e) 
  setwd(dir)
}
