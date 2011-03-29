#' Create learner object.
#' 
#' How to change object later on: Look at setters of \code{\linkS4class{Learner}}.
#' 
#' Tresholds for class labels: If you set \code{predict.type} to "prob" or "decision", the label with the maximum value is selected.
#' You can change labels of a \code{\linkS4class{Prediction}} object later by using the function \code{\link{setThreshold}} or find optimal, 
#' non-default thresholds by using \code{\link{makeProbthWrapper}} and tuning it.
#' 
#' How to add further functionality to a learner: Look at subclasses of \code{\linkS4class{BaseWrapper}}.
#'  
#' @param class [character(1)] \cr
#'        Class of learner to create.
#' @param id [character(1)]\cr 
#'        Id string for object. Used to select the object from a named list, etc.  
#' @param predict.type [character(1)] \cr
#'        Classification: "response" | "prob" | "decision", specifying the type to
#'        predict. Default is "response". "decision" is experimental. Ignored for
#'        regression.	 
#' @param ... [any] \cr
#'        Optional named (hyper)parameters. Alternatively, you can pass via the "par.vals" argument.
#' @param par.vals [list] \cr
#'       Optional list of named (hyper)parameters. Alternatively, you can pass via the ... argument.
#' @return \code{\linkS4class{Learner}}.
#' 
#' @export
#' 
makeLearner = function(class, id, predict.type="response", ..., par.vals=list()) {
	if (class == "")
		stop("Cannot create learner from empty string!")	
	wl = new(class)
  if(!is(wl, "rlearner"))
    stop("Learner must be a basic rlearner!")
	if (!missing(id))
		wl@desc@id = id
  pds = wl@par.set@pars
  # pass defaults
  pv = list()
  for (j in seq(length=length(pds))) {
    pd = pds[[j]]
    if (pd@pass.default) {
      pv[[length(pv)+1]] = pd@default
      names(pv)[length(pv)] = pd@id
    }
  }
  pv = insert(pv, par.vals)
	wl = setHyperPars(wl, ..., par.vals=pv)
  if (predict.type != "response")
    wl = setPredictType(wl, predict.type)
	return(wl)
}
