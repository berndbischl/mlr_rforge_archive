
source("src/mlr/helpers.r")
source("src/runit/helpers.r")
source("src/runit/make.runit.tests.r")

if(!exists("use.package")) {
  use.package = !interactive()
}

if (use.package) {
  message("Using installed copy of mlr for tests!")
  require("mlr")
  require("mlbench")
  require("RUnit")
  ts.dirs = c("src/runit", "src/runit/parallel")  
  ts.file.regexp = "^runit.*"
} else {
  if (file.exists("src/testsuite.config.r")) {
    source("src/testsuite.config.r")    
  } else {
    ts.dirs = "src/runit"
    ts.file.regexp = "^runit.*"
  }
  source("src/files.r")
  load.all.libs()
  load.all.sources("src")
} 


parallel.setup(mode="local")
logger.setup(level="error")
errorhandler.setup()

data(Sonar, BreastCancer)

binaryclass.df <- Sonar
binaryclass.formula <- Class~.
binaryclass.target = "Class"
binaryclass.train.inds <- c(1:50, 100:150)
binaryclass.test.inds  <- setdiff(1:nrow(binaryclass.df), binaryclass.train.inds)
binaryclass.train <- binaryclass.df[binaryclass.train.inds, ]
binaryclass.test  <- binaryclass.df[binaryclass.test.inds, ]
binaryclass.class.col <- 61
binaryclass.class.levs <- levels(binaryclass.df[, binaryclass.class.col])
binaryclass.task = makeClassifTask("binary", data=binaryclass.df, target=binaryclass.target)  

multiclass.df <- iris
multiclass.formula <- Species~.
multiclass.target = "Species"
multiclass.train.inds <- c(1:30, 51:80, 101:130)
multiclass.test.inds  <- setdiff(1:150, multiclass.train.inds)
multiclass.train <- multiclass.df[multiclass.train.inds, ]
multiclass.test  <- multiclass.df[multiclass.test.inds, ]
multiclass.class.col <- 5
multiclass.task = makeClassifTask("multiclass", data=multiclass.df, target=multiclass.target)  

data(BostonHousing)
regr.df <- BostonHousing  
regr.formula <- medv ~ . 
regr.target = "medv"
regr.train.inds <- seq(1, 506, 3)
regr.test.inds  <- setdiff(1:nrow(regr.df), regr.train.inds)
regr.train <- regr.df[regr.train.inds, ]
regr.test  <- regr.df[regr.test.inds, ]
regr.task <- makeRegrTask("regrtask", data=BostonHousing, target="medv")  

.mlr.local$debug.seed <- 12345
debug.seed <<- .mlr.local$debug.seed

testsuite.mlr <- defineTestSuite("mlr",
    dirs = ts.dirs,  
    testFileRegexp = ts.file.regexp
)

testResult <- runTestSuite(testsuite.mlr)

printTextProtocol(testResult)
