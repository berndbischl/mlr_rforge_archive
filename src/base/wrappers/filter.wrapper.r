
#' Wrapper class for learners to filter variables. Experimental. Can currently 
#' only filter to manually selected variables. 
#' 
#' @exportClass filter.wrapper
#' @title Wrapper class for learners to filter variables.

#' @exportClass filter.wrapper
setClass(
		"filter.wrapper",
		contains = c("base.wrapper"),
		representation = representation(
		)
)

#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("filter.wrapper"),
		def = function(.Object, learner, filter.method, filter.threshold) {
      pds = list(
        new("par.desc.disc", par.name="filter.method", 
          vals=c("linear.correlation", "rank.correlation", "information.gain", "gain.ratio", 
            "symmetrical.uncertainty", "chi.squared", "random.forest.importance", "relief", "oneR")),
        new("par.desc.double", par.name="filter.threshold")
      )
      pvs = list(filter.threshold=filter.threshold, filter.method=filter.method)
			callNextMethod(.Object, learner, id=learner["id"], par.descs=pds, par.vals=pvs, pack="FSelector")
		}
)


#' Fuses a base learner with a filter method. Creates a learner object, which can be
#' used like any other learner object. 
#' Currently only filtering to manually selected variables is supported.
#'
#' @param learner [\code{\linkS4class{learner}} or string]\cr 
#'        Learning algorithm. See \code{\link{learners}}.  
#' @param vars [character]\cr 
#'        Selected variables.  
#' 
#' @return \code{\linkS4class{learner}}.
#' 
#' @title Fuse learner with filter method.
#' @export
make.filter.wrapper = function(learner, filter.method="information.gain", filter.threshold) {
  # todo check that for these inputs havew to be all num. or accept error in train and NA in predict?
	new("filter.wrapper", learner=learner, filter.method=filter.method, filter.threshold=filter.threshold)
}



#' @rdname train.learner
setMethod(
		f = "train.learner",
    signature = signature(
      .learner="filter.wrapper", 
      .task="learn.task", .subset="integer"
    ),
		
		def = function(.learner, .task, .subset,  ...) {
      args = list(...)
      method = args$filter.method
      th = args$filter.threshold
      f = as.formula(paste(.targetvar, "~."))
      if (method == "linear.correlation") {
        x = linear.correlation(f, .data)
      } else if (method == "rank.correlation") {
        x = rank.correlation(f, .data)
      } else if (method == "information.gain") {
        x = information.gain(f, .data)
      } else if (method == "gain.ratio") {
        x = gain.ratio(f, .data)
      } else if (method == "symmetrical.uncertainty") {
        x = symmetrical.uncertainty(f, .data)
      } else if (method == "chi.squared") {
        x = chi.squared(f, .data)
      } else if (method == "random.forest.importance") {
        x = random.forest.importance(f, .data)
      } else if (method == "relief") {
        x = relief(f, .data)
      } else if (method == "oneR") {
        x = oneR(f, .data)
      }
      
      imp = x[,1]
      names(imp) = rownames(x)
      vars = names(which(imp > th))
      .data = .data[, c(vars, .targetvar), drop=FALSE]  

      if (length(vars) > 0)
			  m = callNextMethod(.learner, .targetvar, .data, .task.desc, .weights, .costs,  ...)
      else
        m = new("novars", targets=.data[, .targetvar], task.desc=.task.desc)
      # set the vars as attribute, so we can extract it later 
      attr(m, "filter.result") = vars
      return(m)
		}
)

#' @rdname pred.learner

setMethod(
  f = "pred.learner",
  signature = signature(
    .learner = "filter.wrapper", 
    .model = "wrapped.model", 
    .newdata = "data.frame", 
    .type = "character" 
  ),
  
  def = function(.learner, .model, .newdata, .type, ...) {
    vars = .model["vars"] 
    .newdata = .newdata[, vars, drop=FALSE]  
    callNextMethod(.learner, .model, .newdata, .type, ...)
  }
) 

