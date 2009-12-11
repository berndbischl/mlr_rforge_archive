\name{resample.performance}
\alias{resample.performance,learn.task,resample.result,loss,function,function,function-method}
\alias{resample.performance}
\title{performance}
\usage{resample.performance(task, result, loss, aggr1, aggr2, spread)}
\description{Measures the quality of predictions w.r.t. some loss function for a resampled fit.}
\value{A list with the following entries:
\item{\code{values}}{Numeric vector of estimated performances for the resampling iterations.}
\item{\code{aggr}}{Aggregated performance of above performance values. Aggregation is defined by the performance measure, e.g. mean or median.}
\item{\code{spread}}{Spread of above performance values. Defined by the performance measure, e.g. standard deviation.}}
\arguments{\item{task}{[\code{\linkS4class{learn.task}}] \cr
Specifies the learning task for the problem.}
\item{result}{[\code{\linkS4class{resample.result}}] \cr}
\item{measure}{[\code{\link{character}}/\code{\link{list}}] \cr 
Name of performance measure to optimize or a list describing your own performance measure. 
The default is mean misclassification error for classification or MSE for regression.}
}
\examples{library(mlbench)
data(BostonHousing) 
# define a regression task for the Boston Housing problem with a simple linear modell
rt <- make.regr.task("lm", data=iris, target="medv")
# do 3-fold crossvalidation
cv.i <- make.cv.instance(size=nrow(BostonHousing), iters=3) 
rf <- resample.fit(rt, cv.i)
# mean squared error 
resample.performance(rt, rf)
# median of absolute errors
resample.performance(rt, rf, measure="mae")}
