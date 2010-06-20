\name{parallel.setup}
\alias{parallel.setup}
\title{Parallelization setup.}
\usage{parallel.setup(mode="local", parallel.type, cpus, level="resample", ...)
}
\description{Defines the technical mode and level of parallelization when executing code.}
\arguments{\item{mode}{[string] \cr
Which parallel mode should be used: local, multicore, snowfall, sfCluster.}
\item{parallel.type}{[string] \cr
Currently this is only used for sfInit, where it is passed to the 'type' argument. Default is 'MPI'.}
\item{cpus}{[numeric] \cr
Number of requested cpus. Default is mpi.universe.size() for snowfall/MPI, ignored for for sfCluster and 1 otherwise.}
\item{level}{[string] \cr
What is parallelized / what is a job. 
resample: resample.fit is parallelized and a job is train / test.
tune: tune is parallelized and a job is a resampled evaluation of one hyperparameter setting.  
varsel: varsel is parallelized and a job is a resampled evaluation of a feature set.
bench: bennch.exp is parallelized and a job is completely evaluating one learner on one data set.}
\item{...}{[any] \cr
Optional parameters, only passed to sfInit currently.}
}
