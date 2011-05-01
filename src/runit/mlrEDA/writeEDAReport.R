testwriteEDAReport <- function(){
	
	require(brew)
	d.set = iris
	target = "Species"
	name = "iris"
	writeEDAReport(tempdir(),d.set,target,name)
	checkTrue(file.exists("iris.html"))
	checkTrue(file.exists("iris_feat_1.png"))
	checkTrue(file.exists("iris_feat_2.png"))
	checkTrue(file.exists("iris_feat_3.png"))
	checkTrue(file.exists("iris_feat_4.png"))
	checkTrue(file.exists("iris_feat_5.png"))
	checkTrue(file.exists("iris_mds.png"))    
	checkTrue(file.exists("iris_pca.png"))
	
}