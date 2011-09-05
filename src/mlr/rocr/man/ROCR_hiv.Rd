\name{ROCR.hiv}
\alias{ROCR.hiv}
\docType{data}
\title{Data set: Support vector machines and neural networks applied to the
  prediction of HIV-1 coreceptor usage}
  
\description{
  Linear support vector machines (libsvm) and neural networks (R package
  nnet) were applied to predict usage of the coreceptors CCR5 and CXCR4
  based on sequence data of the third variable loop of the HIV envelope
  protein.
}

\usage{data(ROCR.hiv)}

\format{A list consisting of the SVM (\code{ROCR.hiv$hiv.svm}) and NN
  (\code{ROCR.hiv$hiv.nn}) classification data. Each of those is in turn a list
  consisting of the two elements \code{$predictions} and \code{$labels}
  (10 element list representing cross-validation data).}

\references{Sing, T. & Beerenwinkel, N. & Lengauer, T.  "Learning mixtures
  of localized rules by maximizing the area under the ROC curve".  1st
  International Workshop on ROC Analysis in AI, 89-96, 2004.}

\keyword{datasets}
