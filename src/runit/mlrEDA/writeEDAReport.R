testwriteEDAReport <- function(){
  if (!use.package)
    brew.template = file.path("src", "mlrEDA", "writeEDAReport_html.brew") 
  else
    brew.template = NULL
	require(brew)
  tempd = tempdir()
	writeEDAReport(tempd, iris, "Species", "iris", brew.template)

  checkTrue(file.exists(file.path(tempd, "iris.html")))
	checkTrue(file.exists(file.path(tempd, "iris_feat_1.png")))
	checkTrue(file.exists(file.path(tempd, "iris_feat_2.png")))
	checkTrue(file.exists(file.path(tempd, "iris_feat_3.png")))
	checkTrue(file.exists(file.path(tempd, "iris_feat_4.png")))
	checkTrue(file.exists(file.path(tempd, "iris_feat_5.png")))
	checkTrue(file.exists(file.path(tempd, "iris_mds.png")))    
	checkTrue(file.exists(file.path(tempd, "iris_pca.png")))
}
