\name{make.filter.wrapper}
\alias{make.filter.wrapper}
\title{Fuse learner with filter method.}
\usage{make.filter.wrapper(learner, vars)
}
\description{Fuses a base learner with a filter method. Creates a learner object, which can be
used like any other learner object. 
Currently only filtering to manually selected variables is supported.}
\value{\code{\linkS4class{learner}}.}
\arguments{\item{learner}{[\code{\linkS4class{learner}} or string]\cr 
Learning algorithm. See \code{\link{learners}}.}
\item{vars}{[character]\cr 
Selected variables.}
}
