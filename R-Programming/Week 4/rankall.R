rankall <- function(outcome, num="best") {
	data <- read.csv("outcome-of-care-measures.csv")
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
	hosp <- factor()
	rates <- factor()
	stateunq <- data[ ,7]
	stateunq <- stateunq[!duplicated(stateunq)]
	for(i in 1:length(stateunq)) {
		data1 <- subset(data, State==stateunq[i], na.rm=TRUE)
		data1 <- data1[, c(2,7,colnum)]
		data1[data1=="Not Available"] <- NA
		data1 <- na.omit(data1)
		data1[, 3] <- as.numeric(as.character(data1[, 3]))
		data1 <- data1[order(data1[, 3]), ]		
		if(num=="best") {
			hosp <- c(hosp, data1[1,1])
			rates <- c(rates, data1[1,3])
		}
		else if (num=="worst") {
			hosp <- c(hosp, data1[nrow(data1), 1])
			rates <- c(rates, data1[nrow(data1), 3])
		}
		else {
			hosp <- c(hosp, data1[num, 1])
			rates <- c(rates, data1[num, 3])		
		}
	}
	findata <- data.frame("Hospital"=hosp, "State"=stateunq)
	findata <- findata[order(findata[, 2]), ]	
}