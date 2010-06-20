\name{make.res.desc}
\alias{make.res.desc}
\title{Construct resampling description.}
\usage{make.res.desc(method, ...)
}
\description{Generates a description object for a resampling strategy.}
\value{\code{\linkS4class{resample.desc}}.}
\arguments{\item{method}{[string] \cr
"cv" for cross-validation, "bs" for bootstrap, "subsample" for subsampling, "holdout" for holdout}
\item{...}{[any] \cr
Further parameters for strategies. 
iters: Number of resampling iterations.
split: Percentage of training cases for hold-out / subsampling .}
}
