#' A aggregation method reduce the performance values of the test (and possibly the training sets) to a single
#' value. 
#' 
#' Classification (only mmce and acc can be used for multiclass problems): 
#' \itemize{ 
#'    \item{\bold{test.mean}}{\cr Mean of performance values on test sets.}
#'    \item{\bold{test.sd}}{\cr Standard deviation of performance values on test sets.}
#'    \item{\bold{test.median}}{\cr Median of performance values on test sets.}
#'    \item{\bold{test.min}}{\cr Minimum of performance values on test sets.}
#'    \item{\bold{test.max}}{\cr Maximum of performance values on test sets.}
#'    \item{\bold{test.sum}}{\cr Sum of performance values on test sets.}
#'    \item{\bold{train.mean}}{\cr Mean of performance values on training sets.}
#'    \item{\bold{train.sd}}{\cr Standard deviation of performance values on training sets.}
#'    \item{\bold{train.median}}{\cr Median of performance values on training sets.}
#'    \item{\bold{train.min}}{\cr Minimum of performance values on training sets.}
#'    \item{\bold{train.max}}{\cr Maximum of performance values on training sets.}
#'    \item{\bold{train.sum}}{\cr Sum of performance values on training sets.}
#'    \item{\bold{b632}}{\cr Aggregation for B632 bootstrap.}
#'    \item{\bold{b632plus}}{\cr Aggregation for B632+ bootstrap.}
#'    \item{\bold{testgroup.mean}}{\cr Performance values on test sets are grouped according to resampling method. The mean for very group is calculated, then the mean of those means. Mainly used for repeated CV.}
#' }
#' @export 
#' @title Aggregation methods.
aggregations = function() {}


#' @export test.mean
#' @rdname aggregations
test.mean = new("Aggregation",
  id = "test.mean",
  fun = function(perf.test, perf.train, measure, group, pred) mean(perf.test)
)

#' @export test.sd
#' @rdname aggregations
test.sd = new("Aggregation",
  id = "test.sd",
  fun = function(perf.test, perf.train, measure, group, pred) sd(perf.test)
)

#' @export test.median
#' @rdname aggregations
test.median = new("Aggregation",
  id = "test.median",
  fun = function(perf.test, perf.train, measure, group, pred) median(perf.test)
)

#' @export test.min
#' @rdname aggregations
test.min = new("Aggregation",
  id = "test.min",
  fun = function(perf.test, perf.train, measure, group, pred) min(perf.test)
)

#' @export test.max
#' @rdname aggregations
test.max = new("Aggregation",
  id = "test.max",
  fun = function(perf.test, perf.train, measure, group, pred) max(perf.test)
)

#' @export test.sum
#' @rdname aggregations
test.sum = new("Aggregation",
  id = "test.sum",
  fun = function(perf.test, perf.train, measure, group, pred) sum(perf.test)
)


#' @export train.mean
#' @rdname aggregations
train.mean = new("Aggregation",
  id = "train.mean",
  fun = function(perf.test, perf.train, measure, group, pred) mean(perf.train)
)

#' @export train.sd
#' @rdname aggregations
train.sd = new("Aggregation",
  id = "train.sd",
  fun = function(perf.test, perf.train, measure, group, pred) sd(perf.train)
)

#' @export train.median
#' @rdname aggregations
train.median = new("Aggregation",
  id = "train.median",
  fun = function(perf.test, perf.train, measure, group, pred) median(perf.train)
)

#' @export train.min
#' @rdname aggregations
train.min = new("Aggregation",
  id = "train.min",
  fun = function(perf.test, perf.train, measure, group, pred) min(perf.train)
)

#' @export train.max
#' @rdname aggregations
train.max = new("Aggregation",
  id = "train.max",
  fun = function(perf.test, perf.train, measure, group, pred) max(perf.train)
)

#' @export train.sum
#' @rdname aggregations
train.sum = new("Aggregation",
  id = "train.sum",
  fun = function(perf.test, perf.train, measure, group, pred) sum(perf.train)
)




#' @export b632
#' @rdname aggregations
b632 = new("Aggregation",
  id = "b632",
  fun = function(perf.test, perf.train, measure, group, pred) {
    0.632*perf.test + (1-0.632)*perf.train
  }
)

#' @export b632plus
#' @rdname aggregations
b632plus = new("Aggregation",
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
#' @rdname aggregations
testgroup.mean = new("Aggregation",
  id = "testgroup.mean",
  fun = function(perf.test, perf.train, measure, group, pred) {
    mean(sapply(split(perf.test, group), mean))  
  }
)
