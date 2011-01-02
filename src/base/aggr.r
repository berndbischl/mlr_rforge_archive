#' @include object.r
roxygen()



setClass(
  "aggr",
  contains = c("object"),
  representation = representation(
    id = "character",
    fun = "function"
  )
)

#' @rdname measure-class

setMethod(
  f = "[",
  signature = signature("aggr"),
  def = function(x,i,j,...,drop) {
    callNextMethod()
  }
)


#' @export test.mean
test.mean = new("aggr",
  id = "test.mean",
  fun = function(perf.test, perf.train, group, pred) mean(perf.test)
)

#' @export test.sd
test.sd = new("aggr",
  id = "test.sd",
  fun = function(perf.test, perf.train, group, pred) sd(perf.test)
)

#' @export test.median
test.median = new("aggr",
  id = "test.median",
  fun = function(perf.test, perf.train, group, pred) median(perf.test)
)

#' @export test.min
test.min = new("aggr",
  id = "test.min",
  fun = function(perf.test, perf.train, group, pred) min(perf.test)
)

#' @export test.max
test.max = new("aggr",
  id = "test.max",
  fun = function(perf.test, perf.train, group, pred) max(perf.test)
)


#' @export b632
b632 = new("aggr",
  id = "b632",
  fun = function(perf.test, perf.train, group, pred) {
    0.632*perf.test + (1-0.632)*perf.train
  }
)

#' @export b632plus
b632plus = new("aggr",
  id = "b632plus",
  fun = function(perf.test, perf.train, group, pred) {
    y1 = pred["truth"]
    y2 = pred["response"]
    grid = expand.grid(y1, y2, KEEP.OUT.ATTRS=FALSE)
    pred2 = make.prediction(data.desc=pred@data.desc, task.desc=pred@task.desc, 
      id=NULL, truth=grid[,1], type="response", y=grid[,2], group=NULL, 
      threshold=as.numeric(NA), 
      time.train=as.numeric(NA), time.predict=as.numeric(NA)) 
    gamma = performance(pred2, measures=colnames(x), aggr="mean")$measures
    sin = as.numeric(x[which(g == "train"),])
    sout = as.numeric(x[which(g == "test"),])
    R = (sout - sin) / (gamma - sin)
    w = 0.632 / (1 - 0.368*R)
    (1-w) * sin + w*sout
  }
)

#' @export testgroup.mean
testgroup.mean = new("aggr",
  id = "testgroup.mean",
  fun = function(perf.test, perf.train, group, pred) {
    mean(sapply(split(perf.test, group), mean))  
  }
)

