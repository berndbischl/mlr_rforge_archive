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

#' @export test.mean
#' @export aggregations
test.mean = new("aggr",
  id = "test.mean",
  fun = function(perf.test, perf.train, measure, group, pred) mean(perf.test)
)

#' @export test.sd
#' @export aggregations
test.sd = new("aggr",
  id = "test.sd",
  fun = function(perf.test, perf.train, measure, group, pred) sd(perf.test)
)

#' @export test.median
#' @export aggregations
test.median = new("aggr",
  id = "test.median",
  fun = function(perf.test, perf.train, measure, group, pred) median(perf.test)
)

#' @export test.min
#' @export aggregations
test.min = new("aggr",
  id = "test.min",
  fun = function(perf.test, perf.train, measure, group, pred) min(perf.test)
)

#' @export test.max
#' @export aggregations
test.max = new("aggr",
  id = "test.max",
  fun = function(perf.test, perf.train, measure, group, pred) max(perf.test)
)

#' @export test.sum
#' @export aggregations
test.sum = new("aggr",
  id = "test.sum",
  fun = function(perf.test, perf.train, measure, group, pred) sum(perf.test)
)


#' @export train.mean
#' @export aggregations
train.mean = new("aggr",
  id = "train.mean",
  fun = function(perf.test, perf.train, measure, group, pred) mean(perf.train)
)

#' @export train.sd
#' @export aggregations
train.sd = new("aggr",
  id = "train.sd",
  fun = function(perf.test, perf.train, measure, group, pred) sd(perf.train)
)

#' @export train.median
#' @export aggregations
train.median = new("aggr",
  id = "train.median",
  fun = function(perf.test, perf.train, measure, group, pred) median(perf.train)
)

#' @export train.min
#' @export aggregations
train.min = new("aggr",
  id = "train.min",
  fun = function(perf.test, perf.train, measure, group, pred) min(perf.train)
)

#' @export train.max
#' @export aggregations
train.max = new("aggr",
  id = "train.max",
  fun = function(perf.test, perf.train, measure, group, pred) max(perf.train)
)

#' @export train.sum
#' @export aggregations
train.sum = new("aggr",
  id = "train.sum",
  fun = function(perf.test, perf.train, measure, group, pred) sum(perf.train)
)




#' @export b632
#' @export aggregations
b632 = new("aggr",
  id = "b632",
  fun = function(perf.test, perf.train, measure, group, pred) {
    0.632*perf.test + (1-0.632)*perf.train
  }
)

#' @export b632plus
#' @export aggregations
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
      pred2 = makePrediction(task.desc=pred@desc, 
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
#' @export aggregations
testgroup.mean = new("aggr",
  id = "testgroup.mean",
  fun = function(perf.test, perf.train, measure, group, pred) {
    mean(sapply(split(perf.test, group), mean))  
  }
)
