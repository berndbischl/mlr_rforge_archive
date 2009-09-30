\name{set.predict.par}
\alias{set.predict.par-methods}

\title{set.predict.par}
\usage{set.predict.par(object, \ldots)set.predict.par(object, \ldots)}
\description{Set a parameter for the underlying predict function of a 
[\code{\linkS4class{wrapped.learner}}] or - for convienience - a [\code{\linkS4class{learn.task}}].
Used to fix (somewhat techical) arguments which stay the same for the whole experiment.
You should not have to use this too often.}
\alias{set.predict.par,wrapped.learner-method}
\alias{set.predict.par}
\value{\code{set.predict.par,wrapped.learner-method}: Wrapped.learner object with changed paramters for predict function.

}
\arguments{\item{object}{[\code{\linkS4class{wrapped.learner}}] \cr
Wrapping object for the underlying learner.}
\item{\ldots}{Parameters to fix in underlying predict function. Have to be named.}
}
