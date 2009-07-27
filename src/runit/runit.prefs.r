iris.train.inds <- c(1:30, 51:80, 101:130)
iris.test.inds  <- setdiff(1:150, iris.train.inds)

iris.train <- iris[iris.train.inds, ]
iris.test  <- iris[iris.test.inds, ]

debug.seed <- 12345