rankhospital <- function(state, outcome, num="best") {
	data <- read.csv("outcome-of-care-measures.csv")
	if(!(state %in% data[, "State"])) {
		stop("Invalid State")
	}
	if(!(outcome=="heart attack" | outcome=="heart failure" | outcome=="pneumonia")) {
		stop("Invalid Outcome")
	}
	if(outcome=="heart failure") {
		colnum=17
	}
	else if(outcome=="heart attack") {
		colnum=11
	}
	else {
		colnum=23
	}
	data <- subset(data, State==state, na.rm=TRUE)
	data <- data[, c(2,colnum)]
	data[data=="Not Available"] <- NA
	data <- na.omit(data)
	data[, 2] <- as.numeric(as.character(data[, 2]))
	data <- data[order(data[, 2]), ]
	rank <- 1:nrow(data)
	data$rank <- rank
	colnames(data)[2] <- "Rate"
	if(num=="best") {
		exp <- data[1,2]
		indices <- which(data$Rate==exp)
		data_sub <- data[indices, ]
		data_sub <- data_sub[order(data_sub[, 1]), ]
		return(data_sub[1,1])
	}
	else if(num=="worst") {		
		exp <- data[nrow(data),2]
		indices <- which(data$Rate==exp)
		data_sub <- data[indices, ]
		data_sub <- data_sub[order(data_sub[, 1]), ]
		return(data_sub[1,1])
	}
	else {
		exp <- data[num,2]
		indices <- which(data$Rate==exp)
		data_sub <- data[indices, ]
		data_sub <- data_sub[order(data_sub[, 1]), ]
		data_sub
	}
}