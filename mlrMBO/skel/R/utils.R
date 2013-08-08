# Generates MBO task.
#
# @param design [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Initial design.
# @param y.name [\code{character(1)}]\cr
#   Name of y-column for target values in optimization path.
# @return [\code{\link[mlr]{SupervisedTask}]:
#   List of repaired points.
makeMBOTask = function(design, y.name, control) {
  design$dob = design$eol = NULL
  if (any(sapply(design, is.integer)))
    design = as.data.frame(lapply(design, function(x) if(is.integer(x)) as.numeric(x) else x))
  #if (control$rank.trafo)
  #  design[,y.name] = rank(design[,y.name])
  makeRegrTask(target=y.name, data=design)
}

# Repairs points outside of box constraints by clipping it to bounds.
#
# (Sometimes we get one eps below bounds at least after EI)
#
# @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Collection of parameters and their constraints for optimization.
# @param x [\code{list}]\cr
#   List of values which evantually are located below bounds.
# @return [\code{list}]:
#   List of repaired points.
repairPoint = function(par.set, x) {
  Map(function(p, v) {
    if (p$type %in% c("numeric", "numericvector", "integer", "integervector")) {
      if (any(v < p$lower | v > p$upper)) {
        warningf("Repairing value for %s: %s", p$id, as.character(v))
        v = pmax(p$lower, v)
        v = pmin(p$upper, v)
      }
    }
    return(v)
  }, par.set$pars, x)
}
