\name{set.train.par}
\alias{set.train.par,wrapped.learner-method}

\title{Set parameter for training}
\usage{set.train.par(learner, \ldots)}
\description{Set a parameter for the underlying train function of a 
[\code{\linkS4class{wrapped.learner}}].
This is not meant for hyperparameters, pass these through the usual parset argument, but rather to
fix (somewhat technical) arguments which stay the same for the whole experiment. You should not have to use this too often.}
\value{\code{\linkS4class{wrapped.learner}} object with changed parameters for train function of the wrapped learner.}
\arguments{\item{learner}{[\code{\linkS4class{wrapped.learner}}] \cr
The learner.}
\item{\ldots}{Parameters to fix in underlying train function. Have to be named.}
}
