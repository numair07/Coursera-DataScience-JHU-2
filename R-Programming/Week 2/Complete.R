complete <- function(directory, id=1:332) {
	for(i in 1:length(id)) {
		path=directory
		fileList=list.files(path)
		fileNames=as.numeric(sub("\\.csv$","",fileList))
		selectedFiles=fileList[match(id[i],fileNames)]
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
		print(count)
	}
}