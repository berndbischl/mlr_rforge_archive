#todo: threshold for more than 2 classes? cutoff in 

#' Create learner object. 
#' 
#' @param class [string] \cr
#'        Class of learner to create.
#' @param id [string]\cr 
#'        Id string for object. Used to select the object from a named list, etc.  
#' @param predict.type [string] \cr
#'        Classification: "response" | "prob" | "decision", specifying the type to
#'        predict. Default is "response". "decision" is experimental. Ignored for
#'        regression.	 
#' @param ... [any] \cr
#'        Optional named (hyper)parameters. Alternatively, you can pass via the "par.vals" argument.
#' @param par.vals [list] \cr
#'       Optional list of named (hyper)parameters. Alternatively, you can pass via the ... argument.
#' @return \code{\linkS4class{learner}}.
#' 
#' @export
#' 
make.learner = function(class, id, predict.type="response", ..., par.vals=list()) {
	if (class == "")
		stop("Cannot create learner from empty string!")	
	wl = new(class)
	if (!missing(id))
		wl@id = id
  pds = wl@par.descs
  # pass defaults
  pv = list()
  for (j in seq(length=length(pds))) {
    pd = pds[[j]]
    if (pd["pass.default"]) {
      pv[[length(pv)+1]] = pd["default"]
      names(pv)[length(pv)] = pd["par.name"]
    }
  }
  pv = insert(pv, par.vals)
	wl = set.hyper.pars(wl, ..., par.vals=pv)
  wl = set.predict.type(wl, predict.type)
	return(wl)
}
