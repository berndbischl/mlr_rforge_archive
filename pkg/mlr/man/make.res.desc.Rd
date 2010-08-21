\name{make.res.desc}
\alias{make.res.desc}
\title{Construct resampling description.}
\usage{make.res.desc(method, iters, ...)
}
\description{Generates a description object for a resampling strategy.}
\value{\code{\linkS4class{resample.desc}}.}
\arguments{\item{method}{[string] \cr
"cv" for cross-validation, "bs" for bootstrap, "subsample" for subsampling, "holdout" for holdout}
\item{iters}{[integer] \cr
Number of resampling iterations. Not needed for "holdout".}
\item{...}{[any] \cr
Further parameters for strategies. 
split: Percentage of training cases for hold-out / subsampling .}
}
