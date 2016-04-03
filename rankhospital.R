rankhospital <- function(state, outcome, num = "best") {
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
        
        ## Convert state to upper-case
        state <- toupper(state)
        
        ## Check that outcome is valid
        if (outcome != "heart.failure" 
            & outcome != "pneumonia" 
            & outcome != "heart.attack") {stop("invalid outcome")}
        
        ## Attempt to create subset for given state variable
        outcome_data_relevant <- subset(
                outcome_data_small,
                outcome_data_small$state == state
        )
        
        ## Check to see if new subset has any rows (is state valid)
        if (nrow(outcome_data_relevant) == 0) {stop("invalid state")}
        
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
        if (num == "best") {
                as.character(outcome_data_relevant[1,1])
        }
        else if (num == "worst") {
                as.character(outcome_data_relevant[nrow(outcome_data_relevant),1])
        }
        else if (num > nrow(outcome_data_relevant)) {
                NA
        }
        else as.character(outcome_data_relevant[num,1])
        
}