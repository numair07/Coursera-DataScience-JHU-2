corr <- function(directory, threshold) {
	for(i in 1:332) {
		path=directory
		fileList=list.files(path)
		
		fileNames=as.numeric(sub("\\.csv$","",fileList))
		selectedFiles=fileList[match(i,fileNames)]
		data=lapply(file.path(path,selectedFiles),read.csv)
		data=do.call(rbind.data.frame,data)
		flag=0
		count=0
		for(j in 1:nrow(data)) {
			flag=0
			for(k in 1:ncol(data)) {
				if(is.na(data[j,k])) {
					flag=1
					break
				}
			}
			if(flag==0) {
				count <- count+1
			} 	
		}
		data <- data[complete.cases(data), ]
		if(count>threshold) {
			corr <- c(corr,cor(data[ , "nitrate"],data[ , "sulfate"]))
		}
	}
	return(corr)
}