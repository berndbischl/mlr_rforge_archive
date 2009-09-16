\name{conf.matrix}
\alias{conf.matrix}
\title{conf.matrix}
\usage{conf.matrix(learn.task, resample.instance, resample.result, relative)}
\description{\code{conf.matrix} generates the confusion matrix, see \code{\link[klaR]{errormatrix}}.}
\value{A confusion matrix.}
\seealso{\code{\link[klaR]{errormatrix}}}
\arguments{\item{learn.task}{[\code{\linkS4class{learn.task}}] \cr 
Specifies the learning task for the problem.}
\item{resample.instance}{[\code{\linkS4class{resample.instance}}] \cr
Specifies the training and test indices of the resampled data.}
\item{resample.result}{[\code{\linkS4class{resample.result}}] \cr
Result of \code{\link{resample.fit}}, i.e. the perfomed predictions.}
\item{relative}{[logical] \cr 
If TRUE rows are normalized to show relative frequencies.}
}
\examples{data(iris)
ct <- make.classif.task("lda", data=iris, formula=Species~.)
rin <- make.cv.instance(size=nrow(iris), iters=10)
fit <- resample.fit(ct, resample.instance = rin)
conf.matrix(learn.task = ct, resample.instance = rin, resample.result = fit, relative = FALSE)}

