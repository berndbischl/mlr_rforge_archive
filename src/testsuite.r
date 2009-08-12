

source("src/testsuite.config.r")
source("src/files.r")
load.all.libs()
load.all.sources("src")

source("src/runit/helpers.r")
source("src/runit/make.runit.tests.r")




parallel.setup(mode="local", global=TRUE)


logger.define(level="error", global=TRUE)



testsuite.df <- iris
testsuite.formula <- Species~.

testsuite.train.inds <- c(1:30, 51:80, 101:130)
testsuite.test.inds  <- setdiff(1:150, testsuite.train.inds)

testsuite.train <- testsuite.df[testsuite.train.inds, ]
testsuite.test  <- testsuite.df[testsuite.test.inds, ]

testsuite.class.col <- 5


fr <- mlbench.friedman1(150)
fr2 <- as.data.frame(fr$x)
fr2$y <- fr$y 
regr.data <- fr2  
regr.data.train <- regr.data[testsuite.train.inds, ]
regr.data.test  <- regr.data[testsuite.test.inds, ]
regr.formula <- y ~ . 

debug.seed <- 12345
testsuite.mlr <- defineTestSuite("mlr",
  dirs = ts.dirs,  
  testFileRegexp = ts.file.regexp
)

testResult <- runTestSuite(testsuite.mlr)


printTextProtocol(testResult)



#data(Glass)
#testsuite.df <- Glass
#testsuite.formula <- Type~.

#testsuite.train.inds <- seq(1L,214L, 2L)
#testsuite.test.inds  <- seq(2L,214L, 2L)


#
#testsuite.train <- testsuite.df[testsuite.train.inds, ]
#testsuite.test  <- testsuite.df[testsuite.test.inds, ]
#
#testsuite.class.col <- 10
#
#debug.seed <- 12345
#
#testsuite.mlr <- defineTestSuite("mlr",
#  dirs = ts.dirs,  
#  testFileRegexp = ts.file.regexp
#)
#
#testResult <- runTestSuite(testsuite.mlr)
#
#
#printTextProtocol(testResult)
