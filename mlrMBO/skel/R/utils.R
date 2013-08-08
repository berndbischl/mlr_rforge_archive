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
  design = convertDfCols(design, ints.as.num = TRUE)
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
  # FIXME do we need special NA handling here?
  # FIXME removed warning, do we need it?
  Map(function(p, v) {
    if (p$type %in% c("numeric", "numericvector", "integer", "integervector")) {
      #warningf("Repairing value for %s: %s", p$id, as.character(v))
      return(pmax(pmin(v, p$upper), p$lower))
    }
    return(v)
  }, par.set$pars, x)
}
