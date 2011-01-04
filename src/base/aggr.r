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
  fun = function(perf.test, perf.train, measure, group, pred) mean(perf.test)
)

#' @export test.sd
test.sd = new("aggr",
  id = "test.sd",
  fun = function(perf.test, perf.train, measure, group, pred) sd(perf.test)
)

#' @export test.median
test.median = new("aggr",
  id = "test.median",
  fun = function(perf.test, perf.train, measure, group, pred) median(perf.test)
)

#' @export test.min
test.min = new("aggr",
  id = "test.min",
  fun = function(perf.test, perf.train, measure, group, pred) min(perf.test)
)

#' @export test.max
test.max = new("aggr",
  id = "test.max",
  fun = function(perf.test, perf.train, measure, group, pred) max(perf.test)
)


#' @export b632
b632 = new("aggr",
  id = "b632",
  fun = function(perf.test, perf.train, measure, group, pred) {
    0.632*perf.test + (1-0.632)*perf.train
  }
)

#' @export b632plus
b632plus = new("aggr",
  id = "b632plus",
  fun = function(perf.test, perf.train, measure, group, pred) {
    stop(123)
    df = as.data.frame(pred)
    a = numeric(length(perf.test))
    for (i in 1:length(perf.test)) {
      print(i)
      df2 = df[df$iter == i, ]
      y1 = df2$truth
      y2 = df2$response
      grid = expand.grid(y1, y2, KEEP.OUT.ATTRS=FALSE)
      pred2 = make.prediction(task.desc=pred@task.desc, 
        id=NULL, truth=grid[,1], type="response", y=grid[,2],  
        time=as.numeric(NA))
      print(nrow(df2))
      print(nrow(grid))
      gamma = performance(pred2, measure=measure)
      print(gamma)
      R = (perf.test[i] - perf.train[i]) / (gamma - perf.train[i])
      print(R)
      w = 0.632 / (1 - 0.368*R)
      print(w)
      a[i] = (1-w) * perf.train[i] + w*perf.test[i]
    }
    print(a)
    return(mean(a))
  }
)

#' @export testgroup.mean
testgroup.mean = new("aggr",
  id = "testgroup.mean",
  fun = function(perf.test, perf.train, measure, group, pred) {
    mean(sapply(split(perf.test, group), mean))  
  }
)

m = set.aggr(mmce, b632plus)
print(resample("classif.lda", multiclass.task, res, measures=m)$aggr)
