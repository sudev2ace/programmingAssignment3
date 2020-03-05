rankhospital <- function(state,outcome,num = "best") {
        data <- read.csv('outcome-of-care-measures.csv',colClasses = "character")
        st <- state
        if(!st %in% unique(data$State)){
                stop("invalid state")
        }
        if(outcome == "heart attack") {
                newdata <- as.data.frame(cbind(data[,2], data[,7], data[,11]),stringsAsFactors = FALSE)
        } else if(outcome == "pneumonia"){
                newdata <- as.data.frame(cbind(data[,2], data[,7], data[,23]),stringsAsFactors = FALSE)
        } else if(outcome == "heart failure"){
                newdata <- as.data.frame(cbind(data[,2], data[,7], data[,17]),stringsAsFactors = FALSE)
        }
        else{
                stop("invalid outcome")
        }
        
        
        names(newdata) <- c("hospital","state","mortality")
        s <- as.numeric(newdata$mortality)
        newdata$mortality <- s
        newdata <- newdata[complete.cases(newdata),]
        
        stcomp <- newdata$state == st
        newdata <- newdata[stcomp,]
        
        #Now we got the data and its time to arrange them in the order of the mortality rate
        newdata <- newdata[with(newdata, order(newdata$mortality,newdata$hospital)),]
        
        if(num == "best"){
                return(newdata[1,1,1])
        } else if(num == "worst"){
                return(newdata[nrow(newdata),1,1])
        } else if(num > nrow(newdata)){
                return(NA)
        } else  {
                return(newdata[num,1,1])
        }
        
        
}