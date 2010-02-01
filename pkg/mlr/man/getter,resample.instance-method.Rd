\name{getter,resample.instance-method}
\alias{[,resample.instance-method}

\title{Getter for resample.instance}
\description{Getter.}
\alias{resample.instance.getter}
\alias{getter,resample.instance-method}
\arguments{\item{x}{resample.instance object}
\item{i}{[character]
\describe{
\item{size}{Number of observations.}
\item{name}{The name of the resample description object, i.e. the type of resampling.}
\item{iters}{The number of resampling iterations.}
\item{train.inds}{If j is a single integer, the vector of training indices for the jth iteration is returned. If j is an integer vector, the list of training indices for the given iterations is returned. If j is missing, all indices are returned.}
\item{test.inds}{If j is a single integer, the vector of test indices for the jth iteration is returned. If j is an integer vector, the list of test indices for the given iterations is returned. If j is missing, all indices are returned.}
}}
\item{j}{[integer] \cr See above, i == "train.inds" or i == "test.inds".}
}
