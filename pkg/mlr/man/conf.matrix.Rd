\name{conf.matrix}
\alias{conf.matrix}
\title{Confusion matrix}
\usage{conf.matrix(task, result, relative)}
\description{Calculates confusion matrix for test data of a \code{\link{resample.fit}}, 
see \code{\link[klaR]{errormatrix}}.}
\value{A confusion matrix.}
\seealso{\code{\link[klaR]{errormatrix}}}
\arguments{\item{task}{[\code{\linkS4class{learn.task}}] \cr 
Specifies the learning task for the problem.}
\item{result}{[\code{\linkS4class{resample.result}}] \cr
Result of \code{\link{resample.fit}}, i.e. the perfomed predictions.}
\item{relative}{[logical] \cr 
If TRUE rows are normalized to show relative frequencies.}
}
\examples{data(iris)
ct <- make.classif.task(data=iris, target="Species")
res <- make.res.desc("cv", iters=3)
rf <- resample.fit("lda", ct, res)
conf.matrix(ct, rf)}
