\name{train.learner,wrapped.learner,formula,data.frame,numeric,list-method}
\alias{train.learner,wrapped.learner,formula,data.frame,numeric,list-method}

\title{train.learner}
\description{Mainly for internal use. Trains a wrapped learner on a giving training set, 
possibly w.r.t. some hyperparamters and case weights.}
\value{Model of the underlying learner.}
\alias{train.learner}
\arguments{\item{wrapped.learner}{[\code{\link{wrapped.learner}}] \cr  
Wrapped learner from this package.}
\item{formula}{[\code{\link{formula}}] \cr
Specifies inputs and output.}
\item{data}{[\code{\link{data.frame}}] \cr
Training set}
\item{weights}{[\code{\link{numeric}}] \cr
Optional case weights, default is 1.}
\item{parset}{[\code{\link{numeric}}] \cr
Named list of hyperparameters. Default is empty list.}
}
