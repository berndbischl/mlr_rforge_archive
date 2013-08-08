# Optimizers for multipoint infill criteria

# General interface
#
# @param infill.crit [\code{function}]\cr
#   Infill criterion function.
# @param design [\code{data.frame}]\cr
#   Design of already visited points.
# @param model [\code{\link{WrappedModel}}]\cr
#   Model fitted on design.
# @param control [\code{\link{MBOControl}}]\cr
#   Control object.
# @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Parameter set.
# @param opt.path [\code{\link[ParamHelpers{OptPath}}]\cr
#   Optimization path / archive.
# @return [\code{data.frame}]. One proposed point that should be evaluated.

# mean response of model
multipointInfillOptRandom = function(infill.crit.funs, model, control, par.set, opt.path, design) {
	opt.control = control$multipoint.control
  	newdesign = generateDesign(control$seq.design.points, par.set, randomLHS, ints.as.num=TRUE)
  	y = infill.crit(newdesign, model, control, par.set, design)
  	newdesign[sample(nrow(newdesign), control$propose.points), , drop=FALSE]
}
