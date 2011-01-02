
test.prepare <- function(){
  if (!use.package) {
  	test.dataset <- data.frame(1:3, c("bli", "bla", "blub"))
  	
  	pc = prepare.control(ints.as="numeric", chars.as = "factor")
  	ints2nums = prep.data(is.classif=FALSE, data = test.dataset, target = "ints", control=pc)  
  	checkTrue(is.numeric(ints2nums[,1]))
  	
    pc = prepare.control(ints.as="factor", chars.as = "factor")
  	ints2factors = prep.data(is.classif=FALSE, data = test.dataset, target = "ints", control=pc)  
  	checkTrue(is.factor(ints2factors[,1]))
  	
    pc = prepare.control(ints.as="factor", chars.as = "factor")
  	chars2factors <- prep.data(is.classif=FALSE, data = test.dataset, target = "chars", control=pc) 
  	checkTrue(is.factor(chars2factors[,1]))
  	
  	# check inf accessors
  	df = multiclass.df
  	df[1,1:3] = Inf
  	ct = make.task(data=df, target=multiclass.target)	
  	checkTrue(!ct["has.inf"])
    
    # check dropping of levels
    df = multiclass.df[1:60,]
    ct = make.task(data=df, target=multiclass.target)	
    checkEquals(length(levels(df[,multiclass.target])), 3)
    checkEquals(length(unique(ct["targets"])), 2)
    checkEquals(length(levels(ct["targets"])), 2)
  }
}
