\name{set.predict.par}
\alias{set.predict.par,wrapped.learner-method}

\title{Set parameter for prediction}
\usage{set.predict.par(learner, \ldots)}
\description{Set a parameter for the underlying predict function of a wrapped learner. 
Used to fix (somewhat techical) arguments which stay the same for the whole experiment.
You should not have to use this too often.}
\value{Wrapped.learner object with changed paramters for predict function.}
\arguments{\item{learner}{[\code{\linkS4class{wrapped.learner}}] \cr
Wrapping object for the underlying learner.}
\item{\ldots}{Parameters to fix in underlying predict function. Have to be named.}
}
