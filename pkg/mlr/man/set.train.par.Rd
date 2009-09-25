\name{set.train.par}
\alias{set.train.par,learn.task-method}

\title{set.train.par,learn.task-method}
\alias{set.train.par-methods}
\alias{set.train.par}
\usage{set.train.par(object, \ldots)}
\description{Set a parameter for the underlying train function of a wrapped learner. 
This is not meant for hyperparameters, pass these through the usual parset argument, but rather to
fix (somewhat technical) arguments which stay the same for the whole experiment. You should not have to use this too often.}
\value{\code{set.train.par-methods}: learn.task object with changed parameters for train function of the wrapped learner.

}
\alias{set.train.par}
\arguments{\item{object}{[\code{\linkS4class{learn.task}}] \cr
Learn task that contains the wrapped learner.}
\item{\ldots}{Parameters to fix in underlying train function. Have to be named.}
}
\alias{set.train.par,wrapped.learner-method}
\alias{set.train.par}
