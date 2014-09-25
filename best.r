best <- function(state,outname){
	#read outcome data	
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character",
                     na.string = c("No data are available from the hospital for this measure", "Not Available"))
	
	#Check that state and outcome are valid
	staName <- unique(data$State)
	if(!is.element(state,staName)){
		
		stop("invalid state")
	}	
	if(!is.element(outname,c("heart attack","heart failure","pneumonia"))){
		stop("invalid outcome")
	}
	
	#Return hospital name in that state with lowest 30-day death rate
	if(outname=="heart attack"){
	    subdata <- subset(data,State == state)
	    subdata <- subdata[complete.cases(subdata[,11]),]
        a <-as.numeric(subdata[,11])
        m1 <- subdata[order(-a,subdata[,2]),]
        m1[1,2]
	}
#     if(outname=="heart failure"){
#         subdata <- subset(data,State == state)
#         subdata <- subdata[complete.cases(subdata[,17]),]
#         b <-as.numeric(subdata[,17])
#         m2 <- subdata[order(-b,subdata[,2]),]
#         m2[1,2]
#     }
#     if(outname=="pneumonia"){
#         subdata <- subset(data,State == state)
#         subdata <- subdata[complete.cases(subdata[,23]),]
#         c <-as.numeric(subdata[,23])
#         m3 <- subdata[order(-c,subdata[,2]),]
#         m3[1,2]
#     }
}