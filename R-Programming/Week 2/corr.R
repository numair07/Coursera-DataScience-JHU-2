corr <- function(directory, threshold=0) {
	corr1 <- vector()
	for(i in 1:332) {
		path=directory
		fileList=list.files(path)
		
		fileNames=as.numeric(sub("\\.csv$","",fileList))
		selectedFiles=fileList[match(i,fileNames)]
		data=lapply(file.path(path,selectedFiles),read.csv)
		data=do.call(rbind.data.frame,data)
		count <- sum(complete.cases(data))
		data <- data[complete.cases(data), ]
		if(count>threshold) {
			corr1 <- c(corr1,cor(data$nitrate,data$sulfate))
			#print(cor(data$nitrate,data$sulfate))
		}
	}
	#corr1 <- corr1[-1]
	#print(corr1[1, ])
	return(corr1)
}