setwd("/Users/SJY/Desktop/SJY_proj/GitHub/datasciencecoursera/course\ 2/rprog-data-ProgAssignment3-data")

########################################################################################
## Finding the best hospital in a state
best <- function(state, outcome) {
    # Read outcome data
    outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings = "Not Available")

    # Check that state are valid
    if(!state %in% outcomedata$State){
        stop("invalid state")
    }
    
    # Check that outcome are valid
    if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop("invalid outcome")
    }
    
    # Select useful data
    data <- outcomedata[outcomedata$State == state,]
    if(outcome == "heart attack"){
        datause <- data[c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
    }
    else if(outcome == "heart failure"){
        datause <- data[c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
    }
    else{
        datause <- data[c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
    }
    colnames(datause) <- c("name","outcome")
    datause[,"outcome"] <- as.numeric(datause[,"outcome"])
    
    # Return hospital name in that state with lowest 30-day death rate
    datause <- datause[order(datause$outcome,datause$name),]
    best <- datause[1,1]
    return(best)
}

# test
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")


########################################################################################
## Ranking hospitals by outcome in a state
rankhospital <- function(state, outcome, num = "best") {
    # Read outcome data
    outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings = "Not Available")

    # Check that state are valid
    if(!state %in% outcomedata$State){
        stop("invalid state")
    }
    
    # Check that outcome are valid
    if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop("invalid outcome")
    }  
    
    # Select useful data
    data <- outcomedata[outcomedata$State == state,]
    if(outcome == "heart attack"){
        datause <- data[c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
    }
    else if(outcome == "heart failure"){
        datause <- data[c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
    }
    else{
        datause <- data[c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
    }
    colnames(datause) <- c("name","outcome")
    datause[,"outcome"] <- as.numeric(datause[,"outcome"])
    
    # Return hospital name in that state with the given rank 30-day death rate
    datause <- datause[order(datause$outcome,datause$name),]
    if(num == "best"){
        n <- 1
    }
    else if(num == "worst"){
        n <- length(datause$outcome[!is.na(datause$outcome)])
    }
    else{
        n <- num
    }
    result <- datause[n,1]
    return(result)
}

#test
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)


########################################################################################
## Ranking hospitals in all states
rankall <- function(outcome, num = "best") {
    # Read outcome data
    outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings = "Not Available")
    
    # Check that outcome are valid
    if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop("invalid outcome")
    }  
    
    # Select useful data
    if(outcome == "heart attack"){
        data <- outcomedata[c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
    }
    else if(outcome == "heart failure"){
        data <- outcomedata[c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
    }
    else{
        data <- outcomedata[c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
    }
    colnames(data) <- c("name","state","outcome")
    data[,"outcome"] <- as.numeric(data[,"outcome"])
    
    # For each state, find the hospital of the given rank Return a data frame with the hospital names and the (abbreviated) state name
    state <- sort(unique(data$state))
    result <- data.frame(hospital = NA, state = state, row.names = state)
    
    for(i in 1:length(state)){
        datause <- data[data$state == state[i],]
        datause <- datause[order(datause$outcome,datause$name),]
        if(num == "worst"){
            n <- length(datause$outcome[!is.na(datause$outcome)])
        }
        else if(num == "best"){
            n <- 1
        }
        else{
            n <- num
        }
        result[i,1] <- datause[n,1]
    }
    return(result)
}

#test
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)


