outcome <- read.csv("ProgrammingAssignment3/outcome-of-care-measures.csv", 
                    colClasses = "character")

rankhospital <- function(state, outcome_f, num = "best") {
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
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        outcome.tmp.state <- outcome.tmp[which(outcome.tmp$State_ABR == state),
                                         c(1,2,match(outcome_f,names(outcome.tmp)))]
        outcome.tmp.state <- outcome.tmp.state[complete.cases(outcome.tmp.state),]
        outcome.tmp.state <- outcome.tmp.state[order(outcome.tmp.state[,1]),]
        outcome.tmp.state <- outcome.tmp.state[order(outcome.tmp.state[,3]),]
        
        if (num == "best") num <- 1
        if (num == "worst") num <- nrow(outcome.tmp.state)
        
        return(outcome.tmp.state[num,1])
        
        }


