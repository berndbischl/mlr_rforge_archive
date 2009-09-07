\name{kernlab.svm.classif-class}
\alias{kernlab.svm.classif-class}
\alias{kernlab.svm.classif}
\title{kernlab.svm.classif}
\description{Wrapped learner for Support Vector Machines from package \code{kernlab} for classification problems.}
\details{\emph{General hyperparameters:}
\describe{
\item{\code{C}}{Cost of constraints violation (default: 1), this is the 'C'-constant of the regularization term in the Lagrange formulation.}
}
\emph{Kernel hyperparameters, sorted by kernel type:}
\describe{
\item{\code{rbfdot}}{\code{sigma}: inverse kernel width}
\item{\code{laplacedot}}{\code{sigma}: inverse kernel width}
\item{\code{polydot}}{\code{degree}: degree of the polynomial, \cr 
\code{scale}: scaling parameter of the polynomial, \cr 
\code{offset}: offset used in the polynomial}
\item{\code{tanhdot}}{\code{scale}: scaling parameter of the tangent kernel, \cr 
\code{offset}: offset used in the hyperbolic tangent kernel}
\item{\code{besseldot}}{\code{sigma}: inverse kernel width, \cr 
\code{ordner}: order of the Bessel function, \cr
\code{degree}: degree of the Bessel function}
\item{\code{anovadot}}{\code{sigma}: inverse kernel width, \cr 
\code{degree}: degree of the ANOVA kernel function}
\item{\code{stringdot}}{\code{length}: length of the strings considered, \cr
\code{lambda}: the decay factor, \cr 
\code{normalized}: logical parameter determining if the kernel evaluations should be normalized.} 
}
The kernel type and the hyperparameters are specified in \code{parset}.}
\seealso{\code{\link[kernlab]{ksvm}}, \code{\link[kernlab]{dots}}}
\section{Extends}{\code{\linkS4class{wrapped.learner.classif}}}
\section{Methods}{\describe{\item{\code{\link[=initialize,kernlab.svm.classif-method]{initialize}}}{\code{signature( = "kernlab.svm.classif")}: Constructor.}\item{\code{\link[=train.learner,kernlab.svm.classif,formula,data.frame,numeric,list-method]{train.learner}}}{\code{signature(wrapped.learner = "kernlab.svm.classif", formula = "formula", data = "data.frame", weights = "numeric", parset = "list")}: Overwritten, to allow direct passing of kernel hyperparameters.
Besides that, simply delegates to super method.}}}

