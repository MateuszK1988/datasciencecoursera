outcome <- read.csv("ProgrammingAssignment3/outcome-of-care-measures.csv", 
                    colClasses = "character")

rankall <- function(outcome_f, num = "best") {
        ## Read outcome data
        outcome.tmp <- outcome[,c(2,7,11,17,23)]
        colnames(outcome.tmp) <- c("hospital","State_ABR","heart attack", 
                                   "heart failure", "pneumonia")
        
        suppressWarnings(outcome.tmp[,3] <- as.numeric(outcome.tmp[,3]))
        suppressWarnings(outcome.tmp[,4] <- as.numeric(outcome.tmp[,4]))
        suppressWarnings(outcome.tmp[,5] <- as.numeric(outcome.tmp[,5]))
        
        outcome.tmp.state <- unique(outcome.tmp$State_ABR)
        outcome.tmp.state <- sort(outcome.tmp.state)
        
        ## Check that state and outcome are valid
        
        
        if (is.na(match(outcome_f,colnames(outcome.tmp)))) {stop("invalid 
                                                                 outcome")}
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        #table with
        hospV <- vector()
        stateV <- vector()
        for (state in outcome.tmp.state){
                tmp.state.hosp <- outcome.tmp[which(outcome.tmp$State_ABR == state),
                                                 c(1,2,match(outcome_f,names(outcome.tmp)))]
                tmp.state.hosp <- tmp.state.hosp[complete.cases(tmp.state.hosp),]
                tmp.state.hosp <- tmp.state.hosp[order(tmp.state.hosp[,1]),]
                tmp.state.hosp <- tmp.state.hosp[order(tmp.state.hosp[,3]),]
         
                if (num == "best") {
                        num.tmp <- 1
                } else if (num == "worst") {
                        num.tmp <- nrow(tmp.state.hosp)
                } else {
                        num.tmp = as.numeric(num)
                }
                        
                
                hospV <- c(hospV, tmp.state.hosp[num.tmp,]$hospital)
                stateV <-c(stateV,state)
        }
        df <- data.frame(hospital = hospV, state = stateV,row.names = stateV)
        
        return(df)
        
        }