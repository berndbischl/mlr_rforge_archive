
test.prepare <- function(){
  	d = data.frame(a=1:3, b=c("bli", "bla", "blub"), c=c(TRUE, TRUE, FALSE), y=c(1,1,1))
  	
  	pc = prepare.control(ints.as="double", chars.as = "factor", logs.as="factor")
    ct = make.task(data=d, target="y", control=pc) 
    checkEquals(ct["n.feat"], c(1,0,2,0,0), checkNames=FALSE)
    pc = prepare.control(ints.as="factor", chars.as = "factor", logs.as="factor")
    ct = make.task(data=d, target="y", control=pc) 
    checkEquals(ct["n.feat"], c(0,0,3,0,0), checkNames=FALSE)
    pc = prepare.control(ints.as="double", chars.as = "factor", logs.as="double")
    ct = make.task(data=d, target="y", control=pc) 
    checkEquals(ct["n.feat"], c(2,0,1,0,0), checkNames=FALSE)
    pc = prepare.control(ints.as="factor", chars.as = "factor", logs.as="double")
    ct = make.task(data=d, target="y", control=pc) 
    checkEquals(ct["n.feat"], c(1,0,2,0,0), checkNames=FALSE)
    
    pc = prepare.control(ints.as="double", chars.as = "factor", logs.as="factor")
    ct = make.task(data=d, target="b", control=pc) 
    checkEquals(ct["n.feat"], c(2,0,1,0,0), checkNames=FALSE)
    pc = prepare.control(ints.as="factor", chars.as = "factor", logs.as="factor")
    ct = make.task(data=d, target="b", control=pc) 
    checkEquals(ct["n.feat"], c(1,0,2,0,0), checkNames=FALSE)
    pc = prepare.control(ints.as="double", chars.as = "factor", logs.as="double")
    ct = make.task(data=d, target="b", control=pc) 
    checkEquals(ct["n.feat"], c(3,0,0,0,0), checkNames=FALSE)
    pc = prepare.control(ints.as="factor", chars.as = "factor", logs.as="double")
    ct = make.task(data=d, target="b", control=pc) 
    checkEquals(ct["n.feat"], c(2,0,1,0,0), checkNames=FALSE)
    
  	
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
