pollutantmean <- function(directory, pollutant, id=1:332) {
	path=directory
	fileList=list.files(path)
	fileNames=as.numeric(sub("\\.csv$","",fileList))
	selectedFiles=fileList[match(id,fileNames)]
	data=lapply(file.path(path,selectedFiles),read.csv)
	data=do.call(rbind.data.frame,data)
	
	mean(data[, pollutant], na.rm=TRUE)
}