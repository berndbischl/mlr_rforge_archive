\name{resample.performance}
\alias{resample.performance,learn.task,resample.result,loss,function,function,function-method}
\alias{resample.performance}
\title{Performance of a resample.fit}
\usage{resample.performance(task, result, loss, aggr1, aggr2, spread)}
\description{Measures the quality of predictions w.r.t. some loss function for a resampled fit.}
\value{A list with the following entries:
\item{\code{values}}{Numeric vector of estimated performances for the resampling iterations.}
\item{\code{aggr}}{Aggregated performance of above performance values. Aggregation is defined by the performance measure, e.g. mean or median.}
\item{\code{spread}}{Spread of above performance values. Defined by the performance measure, e.g. standard deviation.}}
\arguments{\item{task}{[\code{\linkS4class{learn.task}}] \cr
Learn task.}
\item{result}{[\code{\linkS4class{resample.result}}] \cr
Result from call to \code{\link{resample.fit}}.}
\item{loss}{[\code{\linkS4class{learn.task}}] \cr
Learn task.}
\item{aggr1}{[\code{\link{function}}] \cr
Function used to aggregate performance values on test sets. Default is mean.}
\item{aggr2}{[\code{\link{function}}] \cr
Function used to aggregate indiviual losses of test cases to form a single test set performance value per test set. Default is mean.}
\item{spread}{[\code{\linkS4class{learn.task}}] \cr
Function to calculate spread of performance values of test sets. Default is standard deviation (sd).}
}
\examples{library(mlbench)
data(BostonHousing) 
# define a regression task for the Boston Housing problem with a simple linear modell
rt <- make.regr.task(data=BostonHousing, target="medv")
# do 3-fold crossvalidation
cv.i <- make.res.instance("cv", rt, iters=3) 
rf <- resample.fit("stats.lm", rt, cv.i)
# mean squared error 
resample.performance(rt, rf)
# median of absolute errors on test sets, aggregated by mean
resample.performance(rt, rf, loss="abs")}
