
test.prepare <- function(){
  if (!use.package) {
  	test.dataset <- data.frame(1:3, c("bli", "bla", "blub"))
  	
  	pc = new("prepare.control", props=list(ints.as="numeric", chars.as = "factor"))
  	ints2nums = prep.data(is.classif=FALSE, data = test.dataset, target = "ints", control=pc)  
  	checkTrue(is.numeric(ints2nums[,1]))
  	
  	pc = new("prepare.control", props=list(ints.as="factor", chars.as = "factor"))
  	ints2factors = prep.data(is.classif=FALSE, data = test.dataset, target = "ints", control=pc)  
  	checkTrue(is.factor(ints2factors[,1]))
  	
  	pc = new("prepare.control", props=list(ints.as="factor", chars.as = "factor"))
  	chars2factors <- prep.data(is.classif=FALSE, data = test.dataset, target = "chars", control=pc) 
  	checkTrue(is.factor(chars2factors[,1]))
  	
  	# check inf accessors
  	df = multiclass.df
  	df[1,1:3] = Inf
  	ct = make.task(data=df, target=multiclass.target)	
  	checkTrue(!ct["has.inf"])
  }
}
