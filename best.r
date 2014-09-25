best <- function(state,outname){
	#read outcome data	
	data <- read.csv("outcome-of-care-measures.csv")
	
	#Check that state and outcome are valid
	staName <- unique(data$State)
	filename<-paste("best(",state,",",outname,")")
	if(!is.element(state,staName)){
		
		message("Error in ",filename,":invalid state")
	}	
	if(!is.element(outname,c("heart attack","heart failure","pneumonia"))){
		message("Error in ",filename,":invalid outcome")
	}
	
	#Return hospital name in that state with lowest 30-day death rate
	if(outname=="heart attack"){
	    m <- data[order(data[,11]),]
	}
}