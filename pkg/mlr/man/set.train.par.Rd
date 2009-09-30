\name{set.train.par}
\alias{set.train.par,learn.task-method}

\title{set.train.par}
\description{Set a parameter for the underlying train function of a [\code{\linkS4class{wrapped.learner}}] 
in a [\code{\linkS4class{learn.task}}]. 
This is not meant for hyperparameters, pass these through the usual parset argument, but rather to
fix (somewhat technical) arguments which stay the same for the whole experiment. You should not have to use this too often.}
\value{\code{set.train.par,learn.task-method}: \code{\linkS4class{learn.task}} object with changed parameters for train function of the wrapped learner.

\code{set.predict.par,learn.task-method}: \code{\linkS4class{learn.task}} object with changed parameters for train function of the wrapped learner.


\code{set.train.par,wrapped.learner-method}: \code{\linkS4class{wrapped.learner}} object with changed parameters for train function of the wrapped learner.


}
\arguments{\item{object}{[\code{\linkS4class{learn.task}}] \cr
Learn task that contains the wrapped learner.}
\item{\ldots}{Parameters to fix in underlying train function. Have to be named.}
}
\alias{set.predict.par,learn.task-method}

\alias{set.train.par-methods}
\alias{set.train.par}
\usage{set.train.par(object, \ldots)}
\alias{set.train.par,wrapped.learner-method}
\alias{set.train.par}
