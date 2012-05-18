# FIXME

#
#setMethod(
#   f = "initialize",
#   signature = signature("classif.hda"),
#   def = function(.Object) {
#     
#     setProperties(.Object, 
#         oneclass = FALSE,
#         twoclass = TRUE,
#         multiclass = TRUE,
#         missings = FALSE,
#         numerics = TRUE,
#         factors = FALSE,
#         characters = FALSE,
#         prob = TRUE,
#         weights = FALSE
#     )
#     
#     .Object = callNextMethod(.Object, pack="hda")
#   }
#)
#
##' @rdname trainLearner
#
#setMethod(
#   f = "trainLearner",
#   signature = signature(
#       .learner="classif.hda", 
#       .task="ClassifTask", .subset="integer" 
#   ),
#   
#   def = function(.learner, .task, .subset,  ...) {
#     f = getFormula(.task)
#     hda(f, data=getTaskData(.task, .subset), crule=TRUE, ...)
#   }
#)
#
##' @rdname predictLearner
#
#setMethod(
#   f = "predictLearner",
#   signature = signature(
#       .learner = "classif.hda", 
#       .model = "WrappedModel", 
#       .newdata = "data.frame" 
#   ),
#   
#   #todo wie neue daten vorhersagen?
#   def = function(.learner, .model, .newdata, ...) {
#     .type = ifelse(.type=="response", "class", "raw")
#     m = .model@learner.model
#     predict(m$naivebayes, m$hda.scores, newdata=.newdata, type=.type, ...)
#   }
#)  
#
#
#

