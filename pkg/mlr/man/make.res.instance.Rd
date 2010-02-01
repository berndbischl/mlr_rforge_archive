\name{make.res.instance}
\alias{make.res.instance-methods}
\alias{make.res.instance}
\title{Construct resampling instance}
\description{Generates an instance object for a resampling strategy.}
\value{A \code{\linkS4class{resample.instance}} object.}
\arguments{\item{method}{[\code{\link{integer}}] \cr}
\item{task}{[\code{\link{integer}}] \cr
Data of task to resample from.}
\item{size}{[\code{\link{integer}}] \cr
Size of the data set to resample.}
\item{...}{Futher parameters for strategies.\cr 
iters: Number of generated subsets / resampling iterations.
split: Percentage of training cases for hold-out / subsampling .}
}
