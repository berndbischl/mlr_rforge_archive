require("e1071")
require("mlbench")

source("../skel/R/parameter_space.R")
source("../skel/R/paramils.R")

data(BreastCancer)

data <- na.omit(BreastCancer[,-1])

tuneable_svm <- function(...) {
  l <- list(...)
  n <- nrow(data)
  train <- sample(n, n/2)
  mdl <- svm(Class ~ ., data[train,], ...)
  pred <- predict(mdl, data[-train,])
  mean(pred != data[-train, "Class"])
}


pars <- discrete_parameter_space(kernel=c("linear", "radial", "polynomial"),
                                 degree=c(2, 3, 4),
                                 gamma=c(2^-3, 2^-2, 2^-1, 2, 2^2),
                                 C=c(1, 2, 3, 4, 5))


					res <- parameter_ils(list(kernel="linear", degree=2, gamma=2, C=1),
                     tuneable_svm,
                     pars,
                     control=list(maxeval=500))
