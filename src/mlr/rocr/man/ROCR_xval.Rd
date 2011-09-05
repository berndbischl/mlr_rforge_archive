\name{ROCR.xval}
\alias{ROCR.xval}
\docType{data}
\title{Data set: Artificial cross-validation data for use with ROCR}
  
\description{
  A mock data set containing 10 sets of predictions and
  corresponding labels as would be obtained from 10-fold cross-validation.
}
\usage{data(ROCR.xval)}
\format{A two element list. The first element,
  \code{ROCR.xval$predictions}, is itself a 10 element list. Each of these 10
  elements is a vector of numerical predictions for each cross-validation
  run. Likewise, the second list entry, \code{ROCR.xval$labels} is a 10
  element list in which each element is a vector of true class
  labels corresponding to the predictions.}


\keyword{datasets}
