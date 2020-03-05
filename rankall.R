rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv('outcome-of-care-measures.csv',colClasses = "character")
        
        if(outcome == "heart attack") {
                newdata <- as.data.frame(cbind(data[,2], data[,7], data[,11]),stringsAsFactors = FALSE)
        } else if(outcome == "pneumonia"){
                newdata <- as.data.frame(cbind(data[,2], data[,7], data[,23]),stringsAsFactors = FALSE)
        } else if(outcome == "heart failure"){
                newdata <- as.data.frame(cbind(data[,2], data[,7], data[,17]),stringsAsFactors = FALSE)
        } else{
                stop("invalid outcome")
        }
        
        
        names(newdata) <- c("hospital","state","mortality")
        s <- as.numeric(newdata$mortality)
        newdata$mortality <- s
        newdata <- newdata[complete.cases(newdata),]
        
        newdata <- newdata[with(newdata, order(newdata$mortality,newdata$hospital)),]
        
        
        ## For each state, find the hospital of the given rank
        newdata1 <- split(newdata,newdata$state) ##This is a list
        result <- data.frame("hospital"='',"state"='',stringsAsFactors = FALSE)
        
        
        for(i in 1:length(newdata1)) {
                convdata <- as.data.frame(newdata1[i])
                if (num == "best") {
                       num <- 1
                } else if (num == "worst") {
                        num <- length(convdata)
                }
                result[i,1] <- convdata[num,1]
                result[i,2] <- convdata[1,2]
                
        }
        rownames(result) <- result$state
        result
}
