rankhospital <- function(state,outcome,num = "best"){
    ##Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character",
                     na.string = c("No data are available from the hospital for this measure", "Not Available"))
    
    #Check that state and outcome are valid
    staName <- unique(data$State)
    if(!is.element(state,staName)){
        
        stop("invalid state")
    }    
    if(!is.element(outcome,c("heart attack","heart failure","pneumonia"))){
        stop("invalid outcome")
    }
    
    # 	Return hospital name in that state with lowest 30-day death rate
    
    if(outcome=="heart attack"){
        subdata <- subset(data,State == state)
        subdata <- subdata[complete.cases(subdata[,11]),]
        a<- as.numeric(subdata[,11])
        m <- subdata[order(a,subdata[,2]),]
        if(num=="best") m[1,2]
        else if (num=="worst") m[nrow(m),2]
        else if(num==num) m[num,2]
        else if(num>nrow(m))return(NA)
    }
    else if(outcome=="heart failure"){
        subdata <- subset(data,State == state)
        subdata <- subdata[complete.cases(subdata[,17]),]
        a <- as.numeric(subdata[,17])
        m <- subdata[order(a,subdata[,2]),]
        if(num=="best") m[1,2]
        else if (num=="worst") m[nrow(m),2]
        else if(num==num) m[num,2]
        else if(num>nrow(m)) return(NA)
    }
    else if(outcome=="pneumonia"){
        subdata <- subset(data,State == state)
        subdata <- subdata[complete.cases(subdata[,23]),]
        a <-as.numeric(subdata[,23])
        m <- subdata[order(a,subdata[,2]),]
        if(num=="best") m[1,2]
        else if (num=="worst") m[nrow(m),2]
        else if(num==num) m[num,2]
        else if(num>nrow(m)) return(NA)
    }
   
   
}