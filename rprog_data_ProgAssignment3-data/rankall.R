## rankall takes two arguments: an outcome name (outcome) and a hospital ranking (num)
rankall <- function(outcome, num = "best") {
    ## Read outcome data
    d <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {stop("invalid outcome")}
    
    ## determine the outcome name
    if(outcome == "heart attack")       {n <- 11}
    else if(outcome == "heart failure") {n <- 17}
    else                                {n <- 23} 
    
    ## assign state with all state name value in data and sort it
    state <- sort(unique(d[,7]))
    
    ##remove NA value and transform their types
    d <- d[d[,n] != "Not Available",]
    d[,n] <- as.numeric(d[,n])
    
    ## find all the hospital 
    result <- matrix(nrow = length(state), ncol = 2, dimnames = list(state, c("hospital", "state")))
    for(i in 1:length(state)){
        ##store them to temp
        temp <- data.frame(d[d[,7] == state[i],2], d[d[,7] == state[i],n])
        
        ## sort result by their state value(so that the function can handling the tie)
        ## and their outcome value
        outcome.order <- order(temp[,2], temp[,1])
        
        ## find the hospital's name of the given outcome value
        if(num == "best")       {r <- as.character(temp[outcome.order, 1][1])}
        else if(num == "worst") {r <- as.character(tail(temp[outcome.order, 1], n = 1))}
        else                    {r <- as.character(temp[outcome.order, 1][num])}
        result[i,] = c(r, state[i])
    }
    result
}