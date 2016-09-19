## Author: Emily Quinn Finney
## Programming Assignment 3, Introduction to R
## This module determines the best hospitals for various ailments.

rankhospital <- function(state, outcome, num="best") {
  ## Read outcome data
  care <- read.csv("outcome-of-care-measures.csv",
                   na.strings="Not Available",
                   stringsAsFactors=FALSE)
  hospitaldata <- read.csv("hospital-data.csv",
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
      rate <- filtertable[order(filtertable[,11], filtertable[,2]),]
      
    }
    else if (outcome=="heart failure") {
      # lowest death rate for heart failure
      rate <- filtertable[order(filtertable[,17], filtertable[,2]),]
      }
    else {
      # lowest death rate for pneumonia
      rate <- filtertable[order(filtertable[,23], filtertable[,2]),]
      print(rate[c(2,23)])
      }
  }
  hospitals <- rate[,2]
  print(hospitals)
  if (num=="best") {
    hospital <- hospitals[1]
  }
  else if (num=="worst") {
    hospital <- hospitals[-29]
  }
  else if (num > length(hospitals)) {
    hospital <- NA
  }
  else {
    hospital <- hospitals[num]
  }
  return(hospital)
}