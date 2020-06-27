complete <- function(directory, id=1:332) {
	vect<-0
	for(i in 1:length(id)) {
		path=directory
		fileList=list.files(path)
		fileNames=as.numeric(sub("\\.csv$","",fileList))
		selectedFiles=fileList[match(id[i],fileNames)]
		data=lapply(file.path(path,selectedFiles),read.csv)
		data=do.call(rbind.data.frame,data)
		count <- sum(complete.cases(data))
		vect <- c(vect,count)
	}
	vect <- vect[-1]
	df <- data.frame(id, "nobs"=vect)
	return (df)
}