\name{wrapped.model-class}
\alias{wrapped.model-class}
\alias{wrapped.model}
\title{Induced model of learner.}
\description{Result from \code{\link{train}}. It internally stores the underlying fitted model,
the IDs of the subset used for training, variables used for training and    
information about second-level optimization like tuned hyperparameters or selected variables.}
\details{\code{wrapped.model-class}: Getter.\cr

\describe{
\item{learner [{\linkS4class{learner}}]}{Learner that was used to fit the model.}
\item{learner model [any]}{Underlying model from used R package.}
\item{subset [integer]}{Subset used for training.}
\item{vars [character]}{Variables used for training.}
\item{hyper.pars [list]}{List of fixed hyperparameters and respective values for this model.}
\item{hyper.names [character]}{Names of used hyperparameters.}
\item{hyper.types [character]}{For which step in the model building process the respective hyperparameters used? Named character vector.}
\item{fail [NULL | string]}{Generally NULL but if the training failed, the error message of the underlying train function.}
\item{opt [path.element]}{Optimum of second-level optimization.}
\item{path [list of path.elements]}{Path of second-level optimization.}
\item{tuned.par [list]}{If tuning was performed, best found set of hyperparameters.}
\item{tuned.perf [numeric]}{If tuning was performed, performance of best found set of hyperparameters.}
\item{sel.vars [character]}{If variable selection was performed, best found set of variables.}
\item{sel.perf [numeric]}{If variable selection was performed, performance of best found set of variables.}
}

}
\section{Extends}{\code{\linkS4class{object}}}
\section{Methods}{\describe{\item{\code{\link[=pred.learner,classif.ada,wrapped.model,data.frame,character-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,base.wrapper,wrapped.model,data.frame,ANY-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,classif.blackboost,wrapped.model,data.frame,character-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,regr.blackboost,wrapped.model,data.frame,missing-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,classif.adaboost.M1,wrapped.model,data.frame,character-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,classif.gbm,wrapped.model,data.frame,character-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,regr.gbm,wrapped.model,data.frame,missing-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,classif.glmboost,wrapped.model,data.frame,character-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,classif.grplasso,wrapped.model,data.frame,character-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,classif.J48,wrapped.model,data.frame,character-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,classif.ksvm,wrapped.model,data.frame,character-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,regr.ksvm,wrapped.model,data.frame,missing-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,regr.kknn,wrapped.model,data.frame,missing-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,classif.kknn,wrapped.model,data.frame,character-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,regr.lasso,wrapped.model,data.frame,missing-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,classif.lda,wrapped.model,data.frame,character-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,classif.llr,wrapped.model,data.frame,character-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,regr.lm,wrapped.model,data.frame,missing-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,classif.loclda,wrapped.model,data.frame,character-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,classif.logreg,wrapped.model,data.frame,character-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,classif.lvq1,wrapped.model,data.frame,character-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,classif.mda,wrapped.model,data.frame,character-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,multiclass.wrapper,wrapped.model,data.frame,character-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,classif.multinom,wrapped.model,data.frame,character-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,classif.naiveBayes,wrapped.model,data.frame,character-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,classif.nnet,wrapped.model,data.frame,character-method]{pred.learner}}}{}\item{\code{\link[=predict,wrapped.model-method]{predict}}}{\code{signature(object = "wrapped.model")}: Predict the target variable of new data using a fitted model. If the type is set to "prob" or "decision"
probabilities or decision values will be stored in the resulting object. The resulting class labels are 
the classes with the maximum values or thresholding can also be used.}\item{\code{\link[=pred.learner,preproc.wrapper,wrapped.model,data.frame,character-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,classif.qda,wrapped.model,data.frame,character-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,classif.randomForest,wrapped.model,data.frame,character-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,classif.rda,wrapped.model,data.frame,character-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,regr.ridge,wrapped.model,data.frame,missing-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,classif.rpart,wrapped.model,data.frame,character-method]{pred.learner}}}{}\item{\code{\link[=pred.learner,classif.svm,wrapped.model,data.frame,character-method]{pred.learner}}}{}\item{\code{\link[=to.string,wrapped.model-method]{to.string}}}{}\item{\code{\link[=[,wrapped.model-method]{[}}}{\code{signature( = "wrapped.model")}: Getter.}}}
\alias{[,wrapped.model-method}

