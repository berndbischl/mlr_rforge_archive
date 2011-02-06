summarizeNA  <- function(dataset,show.plot=F){
  
  zahl <- as.numeric(which(apply(is.na(dataset),2,any)))
  if(length(zahl) > 0){
    cat("Variables with NAs: ",colnames(dataset)[zahl],"\n")
    cat("Number of NAs: ",colSums(is.na(dataset[,zahl,drop=F])),"\n")
    
    dataset.new <- dataset[,zahl,drop=F]
    farben <- matrix(0,ncol=ncol(dataset.new),nrow=nrow(dataset.new))
    for(i in 1:nrow(farben)){
      for(j in 1:ncol(farben)){
        if(is.na(dataset.new[i,j])){
          farben[i,j] <- 1
        }   
      }
    }
    if(show.plot){
      par(las=1)
      image(farben,col=c("white","black"))
      abline(v=0)
      abline(h=1.015)
    }
  }
  else{
    cat("There are no missing values in this dataset!!!", "\n")
  }
}

library(mlbench)
data(Soybean)
summarizeNA(Soybean,show.plot=T)


  