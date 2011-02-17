
summaryNA  <- function(dataset,show.plot=F,margin.left=4){
	
	zahl <- as.numeric(which(apply(is.na(dataset),2,any)))
	
	if(length(zahl) > 0){
		
		cat("Variables with NAs: ",colnames(dataset)[zahl],"\n")
		cat("Number of NAs: ",colSums(is.na(dataset[,zahl,drop=F])),"\n")
		
		dataset.new <- dataset[,zahl,drop=F]
		farben <- apply(dataset.new, 2, function(x) as.integer(is.na(x)))
		
		if(show.plot){
			
			image(farben,col=c("white","black"),yaxt="n")
			par(mar=c(5, margin.left, 4, 2) + 0.1)
			abline(v=-0.001)
			abline(h=1)
			
			if(length(zahl) == 1){
				y.type <- 0
			} else {
				y.type <- 0:(ncol(dataset.new)-1)/(length(dataset.new)-1)
			}
			
			axis(2, labels=colnames(dataset.new), at=y.type, las=2)
		}
	}
	
	else{
		cat("There are no missing values in this dataset!!!", "\n")
	}
}


library(mlbench)
data(Soybean)
data(BreastCancer)
summaryNA(Soybean,show.plot=T,margin.left=8)

