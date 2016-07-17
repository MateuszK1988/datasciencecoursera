outcome <- read.csv("ProgrammingAssignment3/outcome-of-care-measures.csv", 
                    colClasses = "character")

best <- function(state, outcome_f) {
        ## Read outcome data
        outcome.tmp <- outcome[,c(2,7,11,17,23)]
        colnames(outcome.tmp) <- c("hospital","State_ABR","heart attack", 
                                   "heart failure", "pneumonia")
                
        suppressWarnings(outcome.tmp[,3] <- as.numeric(outcome.tmp[,3]))
        suppressWarnings(outcome.tmp[,4] <- as.numeric(outcome.tmp[,4]))
        suppressWarnings(outcome.tmp[,5] <- as.numeric(outcome.tmp[,5]))
        
        ## Check that state and outcome are valid
        
        if (is.na(match(state,outcome.tmp$State_ABR))) {stop("invalid state")}
        if (is.na(match(outcome_f,colnames(outcome.tmp)))) {stop("invalid 
                                                                 outcome")}
                
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        outcome.tmp.state <- outcome.tmp[which(outcome.tmp$State_ABR == state),]
        
        rownum <- which.min(outcome.tmp.state[,outcome_f])
        return(outcome.tmp.state[rownum,1])
        
}


