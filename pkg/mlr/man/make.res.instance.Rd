\name{make.res.instance}
\alias{make.res.instance-methods}
\alias{make.res.instance}
\title{Construct resampling instance}
\description{Generates an instance object for a resampling strategy.}
\value{\code{make.res.instance-methods}: A \code{\linkS4class{resample.instance}} object.

}
\arguments{\item{x}{[\code{\link{integer}}] \cr}
\item{task}{[\code{\link{integer}}] \cr
Data of task to resample from.}
\item{size}{[\code{\link{integer}}] \cr
Size of the data set to resample.}
\item{...}{[any] \cr
Further parameters for strategies. 
iters: Number of generated subsets / resampling iterations.
split: Percentage of training cases for hold-out / subsampling .}
}
\alias{make.res.instance,character,missing,numeric-method}
\alias{make.res.instance}
\alias{make.res.instance,character,learn.task,missing-method}
\alias{make.res.instance}
\alias{make.res.instance,resample.desc,missing,numeric-method}
\alias{make.res.instance}
\alias{make.res.instance,resample.desc,learn.task,missing-method}
\alias{make.res.instance}
