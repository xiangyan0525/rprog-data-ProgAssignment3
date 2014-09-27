rankall <- function(outcome,num = "best"){
    ##read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character",
                     na.string = c("No data are available from the hospital for this measure", "Not Available"))
        
    ##Check that state and outcome are valid
    
    if(!is.element(outcome,c("heart attack","heart failure","pneumonia"))){
        stop("invalid outcome")
    }
     ##Return hospital name in that state with lowest 30-day death rate
    staName <- unique(data$State)
    ind <- length(staName)
    hospital <- character(ind)
    state <- character(ind)
    if(outcome=="heart attack"){
        
        for(i in 1:ind){
            #           subdata <- subset(data,State == state)
            spl <- split(data,data$State)
            med <-spl[[i]]
            subdata <- med[complete.cases(med[,11]),]
            a <- as.numeric(subdata[,11])
            m <- subdata[order(a,subdata[,2]),]
            if(num=="best") m[1,2]
            else if (num=="worst") hosname <- m[nrow(m),2]
            else if(num==num) hosname <- m[num,2]
            else if(num>nrow(m)) return(NA)
            hospital[i] <- hosname
            state[i] <- names(spl[i])
        }
        rlt <-data.frame(hospital,state)
        rlt
    }
    else if(outcome=="heart failure"){
        for(i in 1:ind){
            spl <- split(data,data$State)
            med <-spl[[i]]
            subdata <- med[complete.cases(med[,17]),]
            a <- as.numeric(subdata[,17])
            m <- subdata[order(a,subdata[,2]),]
            if(num=="best") hosname <- m[1,2]
            else if (num=="worst") hosname <- m[nrow(m),2]
            else if(num==num) hosname <- m[num,2]
            else if(num>nrow(m)) return(NA)
            hospital[i] <- hosname
            state[i] <- names(spl[i])
        }
        rlt <-data.frame(hospital,state)
        rlt        
    }
    else if(outcome=="pneumonia"){
        for(i in 1:ind){
            #           subdata <- subset(data,State == state)
            spl <- split(data,data$State)
            med <-spl[[i]]
            subdata <- med[complete.cases(med[,23]),]
            a <- as.numeric(subdata[,23])
            m <- subdata[order(a,subdata[,2]),]
            if(num=="best") m[1,2]
            else if (num=="worst") hosname <- m[nrow(m),2]
            else if(num==num) hosname <- m[num,2]
            else if(num>nrow(m)) return(NA)
            hospital[i] <- hosname
            state[i] <- names(spl[i])
        }
        rlt <-data.frame(hospital,state)
        rlt        
    }
 
}