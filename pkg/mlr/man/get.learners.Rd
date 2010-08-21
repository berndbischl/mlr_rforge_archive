\name{get.learners}
\alias{get.learners}
\title{Find learning algorithms with specific properties.}
\usage{get.learners(type=c("classif", "regr"), numerics=NA, factors=NA, characters=NA,
    missings=NA, multiclass=NA, weights=NA, probs=NA, decision=NA,
    costs=NA)
}
\description{Returns the names of learning algorithms which have specific characteristics, e.g.
whether it supports missing values, misclassification costs, case weights,...}
\details{The default of all boolean parameters is NA, meaning: property is not included in search.}
\arguments{\item{type}{[character] \cr
Type of the learning algorithm, either "classif" or "regr"}
\item{numerics}{[boolean] \cr
Supports numeric inputs?}
\item{factors}{[boolean] \cr
Supports factor inputs?}
\item{characters}{[boolean] \cr
Supports character inputs?}
\item{missings}{[boolean] \cr
Supports missing values?}
\item{multiclass}{[boolean] \cr
Supports multiclass problems?}
\item{weights}{[boolean] \cr
Supports case weights?}
\item{probs}{[boolean] \cr
Can predict probabilities?}
\item{decision}{[boolean] \cr
Supports decision values?}
\item{costs}{[boolean] \cr
Supports non-standard misclassification costs?}
}
