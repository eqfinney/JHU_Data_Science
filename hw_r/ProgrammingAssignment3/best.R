## Author: Emily Quinn Finney
## Programming Assignment 3, Introduction to R
## This module determines the best hospitals for various ailments.

best <- function(state, outcome) {
    ## Read outcome data
    care <- read.csv("outcome-of-care-measures.csv",
                     na.strings="Not Available",
                     stringsAsFactors=FALSE)
    hospital <- read.csv("hospital-data.csv",
                         na.strings="Not Available",
                         stringsAsFactors=FALSE)
    emergencies <- c('heart attack','heart failure','pneumonia')
  
    ## Check that state and outcome are valid
    if (!(state %in% care[,7])) {
        stop("invalid state", call.=TRUE)
    }
    else if (!(outcome %in% emergencies)) {
        stop("invalid outcome", call.=TRUE)
    }
    else {
        # find the hospitals in that state
        filtertable <- care[care[,7]==state,]
        # find the lowest value for the death rate for that outcome
        if (outcome=="heart attack") {
            # lowest death rate for heart attack
            rate <- min(na.omit(filtertable[,11]))
            hospital <- filtertable[filtertable[,11]==rate,]
        }
        else if (outcome=="heart failure") {
            # lowest death rate for heart failure
           rate <- min(na.omit(filtertable[,17]))
            hospital <- filtertable[filtertable[,17]==rate,]        }
        else {
            # lowest death rate for pneumonia
            rate <- min(na.omit(filtertable[,23]))
            hospital <- filtertable[filtertable[,23]==rate,]        }
    }
    if (length(hospital[,2]) > 0) {
        hospital_vec <- sort(hospital[,2], decreasing=FALSE)
        hospital <- hospital_vec[1]
    }
    return(hospital)
}