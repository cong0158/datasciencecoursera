## the best function takes two arguments: the 2-character abbreviated name of a state and an
## outcome name, and return the best hospital in the given state with the given outcome name
best <- function(state, outcome){
    ## Read data
    d <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ##Check that state and outcome are valid
    if(!state %in% d[,7]) {stop("invalid state")}
    if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {stop("invalid outcome")}
    
    ## determine the outcome name
    if(outcome == "heart attack")       {num <- 11}
    else if(outcome == "heart failure") {num <- 17}
    else                                {num <- 23} 
    
    ##remove NA value and transform their types
    d <- d[d[,num] != "Not Available",]
    d[, num] <- as.numeric(d[,num])
    
    ##store them to result
    result <- data.frame(d[d[,7] == state,2], d[d[,7] == state,num])
    
    ## sort result by their state value(so that the function can handling the tie)
    ## and their outcome value
    outcome.order <- order(result[,2], result[,1])
    
    ## return the hospital's name of the smallest outcome value
    as.character(result[outcome.order, 1][1])
}