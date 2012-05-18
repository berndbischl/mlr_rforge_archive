
#' Abstract base class for control objects for optimization. 
#' Cannot be instantiated. 
#' 
#' Getter.\cr
#' 
#' @exportClass OptControl
#' @seealso \code{\linkS4class{TuneControl}}, \code{\link[mlrVarsel:VarselControl]{VarselControl}} 
#' @title Base class for control objects for optimization.


makeOptControl = function(path, same.resampling.instance, ...) {
  structure(list(
		path = path,
    same.resampling.instance = same.resampling.instance,
		extra.args = list(...)
  ), class="OptControl")
}
