## rankhospital that takes three arguments: the 2-character abbreviated name of a state (state),
## an outcome (outcome), and the ranking of a hospital in that state for that outcome (num),
## and returns a character vector with the name of the hospital that has the ranking specified by the num argument
rankhospital <- function(state, outcome, num = "best"){
    ## Read data
    d <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ##Check that state and outcome are valid
    if(!state %in% d[,7]) {stop("invalid state")}
    if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {stop("invalid outcome")}
    
    ## determine the outcome name
    if(outcome == "heart attack")       {n <- 11}
    else if(outcome == "heart failure") {n <- 17}
    else                                {n <- 23} 
    
    ##remove NA value and transform their types
    d <- d[d[,n] != "Not Available",]
    d[,n] <- as.numeric(d[,n])
    
    ##store them to result
    result <- data.frame(d[d[,7] == state,2], d[d[,7] == state,n])
    
    ## sort result by their state value(so that the function can handling the tie)
    ## and their outcome value
    outcome.order <- order(result[,2], result[,1])
    
    ## find the hospital's name of the given outcome value
    if(num == "best")       {r <- as.character(result[outcome.order, 1][1])}
    else if(num == "worst") {r <- as.character(tail(result[outcome.order, 1], n = 1))}
    else                    {r <- as.character(result[outcome.order, 1][num])}
    
    r
}