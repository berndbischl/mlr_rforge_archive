\name{tune}
\alias{tune}
\title{tune}
\usage{tune(learn.task, resample.instance, ranges, measure="mmce")}
\description{tune optimizes the hyperparameters of a learner for a classification or regression problem. 
Given some ranges for one or more hyperparameters, it estimates the performance 
of the learner for each possible combination of the proposed values by 
using a resampling method (e.g. cross-validation) and returns the best parameter set and its performance.}
\value{A list containing the best parameter set, its aggregated performance over all resampling iterations, a measure of spread and 
a data frame containing the same values for all evaluated combinations of parameter values.}
\arguments{\item{learn.task}{[\code{\linkS4class{learn.task}}] \cr
Specifies the learning task for the problem}
\item{resample.instance}{[\code{\linkS4class{resample.instance}}] \cr
Specifies the training and test indices of the resampled data.}
\item{ranges}{[list]\cr A list of named vectors of possible values for each hyperparamter. 
You can also pass a list of such list if it does not make sense to search a complete 
cross-product of range values.}
\item{measure}{[character/list] \cr Name of perfomrnace measure to optimize or a list describing your own performance measure. The default is mean misclassification error.}}
\examples{library(mlr) 
ct <- make.classif.task(learner="kernlab.svm.classif", data=iris, formula=Species~.)
ranges <- list(kernel="rbfdot", C=2^seq(-1,1), sigma=2^seq(-1,1))
ri <- make.cv.instance(size=nrow(iris), iters=3)  
tune(learn.task=ct, resample.instance=ri, ranges=ranges)}
