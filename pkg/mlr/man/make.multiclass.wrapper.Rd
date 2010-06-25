\name{make.multiclass.wrapper}
\alias{make.multiclass.wrapper}
\title{Fuse learner with multiclass method.}
\usage{make.multiclass.wrapper(learner, ...)
}
\description{Fuses a base learner with a multi-class method. Creates a learner object, which can be
used like any other learner object. This way learners which can only handle binary classification 
will be able to handle multi-class problems too.
Currently only "one-vs-all" is implemented.}
\value{\code{\linkS4class{learner}}.}
\arguments{\item{learner}{[\code{\linkS4class{learner}} or string]\cr 
Learning algorithm. See \code{\link{learners}}.}
\item{...}{[any] \cr
Optional parameters. Not used currently.}
}
