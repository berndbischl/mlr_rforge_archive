##' @include learnerR.r
#roxygen()
##' @include wrapped.model.r
#roxygen()
##' @include train.learner.r
#roxygen()
##' @include pred.learner.r
#roxygen()
#
#
#setClass(
#		"classif.hda", 
#		contains = c("rlearner.classif")
#)
#
#
#setMethod(
#		f = "initialize",
#		signature = signature("classif.hda"),
#		def = function(.Object) {
#			
#			desc = new("learner.desc.classif",
#					oneclass = FALSE,
#					twoclass = TRUE,
#					multiclass = TRUE,
#					missings = FALSE,
#					numerics = TRUE,
#					factors = FALSE,
#					characters = FALSE,
#					probs = TRUE,
#					decision = FALSE,
#					weights = FALSE,
#					costs = FALSE
#			)
#			
#			callNextMethod(.Object, pack="hda", desc=desc)
#		}
#)
#
##' @rdname train.learner
#
#setMethod(
#		f = "train.learner",
#		signature = signature(
#				.learner="classif.hda", 
#				.targetvar="character", 
#				.data="data.frame", 
#				.data.desc="data.desc", 
#				.task.desc="task.desc", 
#				.weights="numeric", 
#				.costs="matrix" 
#		),
#		
#		def = function(.learner, .targetvar, .data, .data.desc, .task.desc, .weights, .costs,  ...) {
#			f = as.formula(paste(.targetvar, "~."))
#			hda(f, data=.data, crule=TRUE, ...)
#		}
#)
#
##' @rdname pred.learner
#
#setMethod(
#		f = "pred.learner",
#		signature = signature(
#				.learner = "classif.hda", 
#				.model = "wrapped.model", 
#				.newdata = "data.frame", 
#				.type = "character" 
#		),
#		
#		#todo wie neue daten vorhersagen?
#		def = function(.learner, .model, .newdata, .type, ...) {
#			.type = ifelse(.type=="response", "class", "raw")
#			m = .model["learner.model"]
#			predict(m$naivebayes, m$hda.scores, newdata=.newdata, type=.type, ...)
#		}
#)	
#
#
#

