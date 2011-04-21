##' Implement this for your own sequential resampling.
##' @exportMethod resample.update
##' @rdname resample.update
#
#setGeneric(
#		name = "resample.update",
#		def = function(x, task, model, pred) {
#			standardGeneric("resample.update")
#		}
#)
#
#
#
##' @rdname resample.update
##' @export
#setMethod(
#		f = "resample.update",
#		signature = signature("ResampleInstance", "LearnTask", "WrappedModel", "Prediction"),
#		def = function(x, task, model, pred) {
#			return(x)
#		}
#)
#
##' Implement this for your own sequential resampling.
##' @exportMethod resample.done
##' @rdname resample.done
#
#setGeneric(
#		name = "resample.done",
#		def = function(x, task, model, pred) {
#			standardGeneric("resample.done")
#		}
#)
#
#
##' @rdname resample.done
##' @export
#
#setMethod(
#		f = "resample.done",
#		signature = signature("ResampleInstance"),
#		def = function(x, task, model, pred) {
#			return(x)
#		}
#)
#

