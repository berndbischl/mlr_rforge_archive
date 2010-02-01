\name{tune}
\alias{tune}
\title{Hyperparameter tuning}
\usage{tune(learner, task, resampling, fixed=list(), method="grid", control=NULL, loss, model=F, scale=I)}
\description{Optimizes the hyperparameters of a learner for a classification or regression problem.
Allows for different optimization methods, commonly grid search is used but other search techniques
are available as well.  
Given some ranges for one or more hyperparameters, it estimates the performance
of the learner for each possible combination of the proposed values by
using a resampling method (e.g. cross-validation) and returns the best parameter set and its
performance.}
\value{A list. Might contain some additional information from the optimizer and at least:
\item{par}{Named list of best found hyperparamters.}
\item{perf}{Best found performance value.}
\item{model}{Fitted model on complete data set - if requested.}}
\arguments{\item{learner}{[\code{\linkS4class{wrapped.learner}}] \cr
Learning method.}
\item{task}{[\code{\linkS4class{learn.task}}] \cr
Learning task.}
\item{resampling}{[\code{\linkS4class{resample.instance}}] or [\code{\linkS4class{resample.desc}}]\cr
Resampling strategy to evaluate points in hyperparameter space.}
\item{fixed}{[\code{\link{list}}] \cr
Named list of hyperparameter values which are kept fixed during the optimization. Default is list().}
\item{method}{[\code{\link{character}}] \cr
Search method. Currently supported are "grid", "pattern", "cmaes".}
\item{control}{Control object for search method.}
\item{loss}{[\code{\linkS4class{loss}}] or [\code{\link{character}}]\cr
Loss to use for tuning. Default is "zero-one" for classification and "squared" error for regression.}
\item{model}{[\code{\link{logical}}]\cr
Should a final model be fitted on the complete data with the best found hyperparameters?}
\item{scale}{[\code{\link{function}}]
A function to scale the hyperparamters. E.g. maybe you want to optimize in some log-space.
Has to take a single, numerical vector and return a scaled one. Default is identity function.}
}
\examples{ct <- make.classif.task(data=iris, target="Species")
r <- list(C=2^(-1:1), sigma=2^(-1:1))
res <- make.res.desc("cv", iters=3)
tune("kernlab.svm.classif", ct, res, control=grid.control(ranges=r))}
