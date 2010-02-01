\name{bench.add}
\alias{bench.add}
\title{Add learner to benchmark experiment}
\usage{bench.add(learner, task, result)}
\description{Add another learner to a benchmark experiment.
The same resampling pairing is used as for the other learners.}
\value{[\code{\linkS4class{bench.result}}]}
\seealso{\code{\link{bench.exp}}}
\arguments{\item{learner}{[\code{\linkS4class{wrapped.learner}} or \code{\link{character}}] \cr
Added learner.}
\item{task}{[\code{\linkS4class{learn.task}}] \cr
Learning task.}
\item{result}{[\code{\linkS4class{bench.result}}] \cr
Result of previous experiment.}
}
