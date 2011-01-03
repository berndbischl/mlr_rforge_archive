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
#			desc = c(
#					oneclass = FALSE,
#					twoclass = TRUE,
#					multiclass = TRUE,
#					missings = FALSE,
#					doubles = TRUE,
#					factors = FALSE,
#					characters = FALSE,
#					prob = TRUE,
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
#				.task="classif.task", .subset="integer", .vars="character" 
#		),
#		
#		def = function(.learner, .task, .subset, .vars,  ...) {
#			f = .task["formula"]
#			hda(f, data=get.data(.task, .subset, .vars), crule=TRUE, ...)
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

