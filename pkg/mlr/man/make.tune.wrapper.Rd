\name{make.tune.wrapper}
\alias{make.tune.wrapper}
\title{Fuse learner with tuning.}
\usage{make.tune.wrapper(learner, resampling, control, measures, aggr)
}
\description{Fuses a base learner with a search strategy to select its hyperparameters. Creates a learner object, which can be
used like any other learner object, but which internally uses tune. If the train function is called on it, the search strategy and resampling are invoked
to select an optimal set of hyperparameter values. Finally, a model is fitted on the complete training data with these optimal
hyperparameters and returned.}
\value{\code{\linkS4class{learner}}.}
\seealso{\code{\link{tune}}, \code{\link{grid.control}}, \code{\link{cmaes.control}}, \code{\link{neldermead.control}}}
\arguments{\item{learner}{[\code{\linkS4class{learner}} or string]\cr 
Learning algorithm. See \code{\link{learners}}.}
\item{resampling}{[\code{\linkS4class{resample.instance}}] or [\code{\linkS4class{resample.desc}}]\cr
Resampling strategy to evaluate points in hyperparameter space.}
\item{control}{Control object for search method. Also selects the optimization algorithm for tuning.}
\item{measures}{[see \code{\link{measures}}]
Performance measures.}
\item{aggr}{[see \code{\link{aggregations}}]
Aggregation functions.}
}
