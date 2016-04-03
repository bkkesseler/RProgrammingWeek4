rankall <- function(outcome, num = "best") {
        ## Read outcome data    
        outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Converting the heart failure data to numeric, warning of NA creation 
        ## ignored
        suppressWarnings(outcome_data[, 11] <- as.numeric(outcome_data[, 11]))
        
        ## Converting the pneumonia data to numeric, warning of NA creation 
        ## ignored
        suppressWarnings(outcome_data[, 17] <- as.numeric(outcome_data[, 17]))
        
        ## Converting the heart attack data to numeric, warning of NA creation 
        ## ignored
        suppressWarnings(outcome_data[, 23] <- as.numeric(outcome_data[, 23]))
        
        ## Subset to the relevant data
        outcome_data_small <- data.frame(
                "hospital" = outcome_data[,2],
                "state" = outcome_data[,7],
                "heart attack" = outcome_data[,11],
                "heart failure" = outcome_data[,17],
                "pneumonia" = outcome_data[,23]
        )
        
        ## Convert outcome to lower-case, replace " " with "."
        outcome <- tolower(outcome)
        outcome <- gsub(" ",".",outcome)
        
        ## Check that outcome is valid
        if (outcome != "heart.failure" 
            & outcome != "pneumonia" 
            & outcome != "heart.attack") {stop("invalid outcome")}
        
        ## Create list of valid states, then sort it
        state_list <- unique(outcome_data_small$state)
        state_list <- sort(state_list)
        
        ## Loop over every state
        ##for (i in 1:5) {
        for (i in 1:length(state_list)) {
                ## Attempt to create subset for given state variable
                outcome_data_relevant <- subset(outcome_data_small,
                        outcome_data_small$state == state_list[i]
                        )
                
                ## Subset further
                outcome_data_relevant <- data.frame(
                        "hospital" = outcome_data_relevant$hospital,
                        "state" = outcome_data_relevant$state,
                        "outcome" = outcome_data_relevant[[outcome]]
                )
                
                ## Remove NAs
                outcome_data_relevant <- na.omit(outcome_data_relevant)
                
                ## Sort data frame by outcome, then alphabetically
                outcome_data_relevant <- outcome_data_relevant[
                        order(outcome_data_relevant$outcome,
                              outcome_data_relevant$hospital,
                              na.last = TRUE),
                        ]        
                
                ## Return hospital name in that state with the given rank
                ## 30-day death rate
                outcome_data_relevant <- 
                       if (num == "best") {
                                data.frame("hospital" = as.character(outcome_data_relevant[1,1]),
                                  "state" = as.character(state_list[i]))
                                }
                        else if (num == "worst") {
                                data.frame("hospital" = as.character(outcome_data_relevant[nrow(outcome_data_relevant),1]),
                                  "state" = as.character(state_list[i]))
                                }
                        else if (num > nrow(outcome_data_relevant)) {
                                data.frame("hospital" = NA,"state" = as.character(state_list[i]))
                }
                else data.frame("hospital" = as.character(outcome_data_relevant[num,1]),
                       "state" = as.character(state_list[i]))
                
                outcome_data_new <- data.frame(outcome_data_relevant)
                row.names(outcome_data_new)[1] <- as.character(state_list[i])
                
                if (i == 1) {
                        outcome_data <- outcome_data_new
                }
                else {
                        outcome_data <- rbind(outcome_data,outcome_data_new)
                }
        }
        
        
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        outcome_data
}