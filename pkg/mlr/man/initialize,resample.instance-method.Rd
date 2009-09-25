\name{initialize,resample.instance-method}
\alias{initialize,resample.instance-method}

\title{This is mainly for internal use, you only need to use this, when you extend resample...}
\description{This is mainly for internal use, you only need to use this, when you extend resample.instance to add another resampling strategy!}
\arguments{\item{desc}{[\code{\linkS4class{resample.desc}}] \cr resample.desc. Describes the resampling strategy.}
\item{size}{[integer] \cr Size of the data set to resample from.}
\item{inds}{[list of integer vectors] \cr Indices of the trainings sets.}
}
