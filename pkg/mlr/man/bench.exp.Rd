\name{bench.exp}
\alias{bench.exp}
\title{Bencnmark experiment for multiple learners}
\usage{bench.exp(learners, tasks, resampling)}
\description{Complete benchmark experiment for a learn task.
Allows you to compare a list of learning algorithms by measuring the test error w.r.t. to a given resampling strategy.  
Experiments are paired, meaning always the same training / test sets are used for the different learners.}
\value{A matrix of test error. Columns correspond to learners, row to the iteration of the resampling strategy.}
\note{You can also get automatic, internal tuning by using \code{\link{make.tune.wrapper}} with your learner.}
\seealso{\code{\link{bench.add}}, \code{\link{make.tune.wrapper}}}
\alias{bench.exp}
\arguments{\item{learners}{[\code{\link{list}} of \code{\linkS4class{wrapped.learner}} or \code{\link{character}}] \cr
Defines the learning algorithms which should be compared.}
\item{task}{[\code{\linkS4class{learn.task}}] \cr
Learning task.}
\item{resampling}{[\code{\linkS4class{resample.desc}} or \code{\linkS4class{resample.instance}}] \cr
Resampling strategy.}
}
\examples{ct <- make.classif.task(data=iris, target="Species")
# very small grid for svm hyperpars 
r <- list(C=2^seq(-1,1), sigma=2^seq(-1,1))
inner.res <- make.res.desc("cv", iters=3)   
svm.tuner <- make.tune.wrapper("kernlab.svm.classif", method="grid", resampling=inner.res, control=grid.control(ranges=r))
learners <- c("lda", "qda", svm.tuner)
res <- make.res.desc("cv", iters=5)
bench.exp(learners, ct, res)}
