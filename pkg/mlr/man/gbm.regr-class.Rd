\name{gbm.regr-class}
\alias{gbm.regr-class}
\alias{gbm.regr}
\title{gbm.regr}
\description{Wrapped learner for Gradient Boosting Machine from package \code{gbm} for regression problems.}
\details{\emph{Common hyperparameters:}
\describe{
\item{\code{n.trees}}{Total number of trees to fit. This is equivalent to the number of iterations and the number of basis functions in the additive expansion.}			
\item{\code{interaction.depth}}{The maximum depth of variable interactions. 1 implies an additive model, 2 implies a model with up to 2-way interactions, etc.}
\item{\code{n.minobsinnode}}{Minimum number of observations in the trees terminal nodes.}
\item{\code{shrinkage}}{Shrinkage parameter applied to each tree in the expansion. Also known as the learning rate or step-size reduction.}			
\item{\code{bag.fraction}}{Fraction of the training set observations randomly selected to propose the next tree in the expansion.}
}}
\seealso{\code{\link[gbm]{gbm}}}
\section{Extends}{\code{\linkS4class{wrapped.learner.regr}}}
\section{Methods}{\describe{}}
