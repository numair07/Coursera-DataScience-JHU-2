best <- function(state, outcome) {
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
	a <- suppressWarnings(min(as.numeric(data[, colnum]), na.rm=TRUE))
	#print(a)
	rownum <- suppressWarnings(which(as.numeric(data[, colnum])==a, arr.ind=TRUE))
	return(data[rownum,2])
}	