
test.prepare <- function(){
    pc = prepare.control()
    checkEquals(length(pc@ints.as), 1)
    checkEquals(length(pc@chars.as), 1)
    checkEquals(length(pc@logs.as), 1)
    checkEquals(length(pc@Dates.as), 1)
    checkEquals(length(pc@POSIXcts.as), 1)

    d = data.frame(a=1:3, b=c("bli", "bla", "blub"), c=c(TRUE, TRUE, FALSE), 
      d=as.Date(c("1970-01-01", "1970-01-02", "1970-01-03")), 
      e=as.POSIXct(c("1970-01-01", "1970-01-02", "1970-01-03")), 
      y=c(1,1,1))
  	
  	pc = prepare.control(ints.as="numeric", chars.as = "factor", logs.as="factor")
    ct = makeRegrTask(data=d, target="y", control=pc) 
    checkEquals(ct["n.feat"], c(3,0,2,0,0), checkNames=FALSE)
    checkTrue(is.numeric(get.data(ct)$d))
    checkTrue(is.numeric(get.data(ct)$e))
    checkEquals(get.data(ct)$d, get.data(ct)$e)
    
    pc = prepare.control(ints.as="factor", chars.as = "factor", logs.as="factor", POSIXcts.as="minutes")
    ct = makeRegrTask(data=d, target="y", control=pc) 
    checkEquals(ct["n.feat"], c(2,0,3,0,0), checkNames=FALSE)
    checkEquals(get.data(ct)$d, get.data(ct)$e * 60)
    
    pc = prepare.control(ints.as="numeric", chars.as = "factor", logs.as="numeric")
    ct = makeRegrTask(data=d, target="y", control=pc) 
    checkEquals(ct["n.feat"], c(4,0,1,0,0), checkNames=FALSE)
    pc = prepare.control(ints.as="factor", chars.as = "factor", logs.as="numeric")
    ct = makeRegrTask(data=d, target="y", control=pc) 
    checkEquals(ct["n.feat"], c(3,0,2,0,0), checkNames=FALSE)
    
    pc = prepare.control(ints.as="numeric", chars.as = "factor", logs.as="factor")
    ct = makeClassifTask(data=d, target="b", control=pc) 
    checkEquals(ct["n.feat"], c(4,0,1,0,0), checkNames=FALSE)
    pc = prepare.control(ints.as="factor", chars.as = "factor", logs.as="factor")
    ct = makeClassifTask(data=d, target="b", control=pc) 
    checkEquals(ct["n.feat"], c(3,0,2,0,0), checkNames=FALSE)
    pc = prepare.control(ints.as="numeric", chars.as = "factor", logs.as="numeric")
    ct = makeClassifTask(data=d, target="b", control=pc) 
    checkEquals(ct["n.feat"], c(5,0,0,0,0), checkNames=FALSE)
    pc = prepare.control(ints.as="factor", chars.as = "factor", logs.as="numeric")
    ct = makeClassifTask(data=d, target="b", control=pc) 
    checkEquals(ct["n.feat"], c(4,0,1,0,0), checkNames=FALSE)
    
  	
  	# check inf accessors
  	df = multiclass.df
  	df[1,1:3] = Inf
  	ct = makeClassifTask(data=df, target=multiclass.target)	
  	checkTrue(!ct["has.inf"])
    
    # check dropping of levels
    df = multiclass.df[1:60,]
    ct = makeClassifTask(data=df, target=multiclass.target)	
    checkEquals(length(levels(df[,multiclass.target])), 3)
    checkEquals(length(unique(getTargets(ct))), 2)
    checkEquals(length(levels(getTargets(ct))), 2)
    
    # check replacement of special chars in colnames
    mydata = multiclass.df
    colnames(mydata)[5] = "y+z"
    checkException(makeClassifTask(data=mydata, target="y+z"), silent=TRUE)
    s = geterrmessage()
    checkTrue(length(grep("You have to rename", s)) >0 )
    colnames(mydata)[1:2] = c("foo[bar]", "foo+bar")
    colnames(mydata)[5] = multiclass.target
    checkWarning(makeClassifTask(data=mydata[multiclass.train.inds,], target=multiclass.target), 
      "Converting feature names containing ")  
    ct = makeClassifTask(data=mydata[multiclass.train.inds,], target=multiclass.target)
    checkEquals(getFeatureNames(ct), c("foo91bar93", "foo43bar", "Petal.Length", "Petal.Width"))
}
