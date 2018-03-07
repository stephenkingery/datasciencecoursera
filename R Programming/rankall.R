setwd("C:/Users/stephen.l.kingery/Desktop/Coursera/rprog_data_ProgAssignment3-data")

rankall <- function(outcome, num = "best") { 
  ## Read outcome data
  outcome_dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
  outcome_dat[, 11] <- as.numeric(outcome_dat[, 11])
  outcome_dat[, 17] <- as.numeric(outcome_dat[, 17])  
  outcome_dat[, 23] <- as.numeric(outcome_dat[, 23])
  
  ## Check that state and outcome are valid
  valid <- c("heart attack", "heart failure", "pneumonia")
  if (is.element(outcome, valid) == FALSE) {
    stop("invalid outcome")
  }
  
  ## For each state, find the hospital of the given rank
  if (outcome == "heart attack") {
    outcome_dat$Outcome <- outcome_dat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
  }
  else if (outcome == "heart failure") {
    outcome_dat$Outcome <- outcome_dat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
  }
  else {
    outcome_dat$Outcome <- outcome_dat$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
  }
  
  outcome_dat <- outcome_dat[c("Hospital.Name","State","Outcome")]
  outcome_dat <- outcome_dat[complete.cases(outcome_dat),]
  outcome_dat <- outcome_dat[order(outcome_dat$State, outcome_dat$Outcome, outcome_dat$Hospital.Name),]
  
  state_split <- split(outcome_dat, outcome_dat$State) 
  head(state_split)
  
  rank <- lapply(state_split, function(x, num) {
    if (num == "best") {
      num <- 1
    }
    else if (num =="worst") {
      num <- nrow(x)
    }
    x$Hospital.Name[num]
  }, num)
  
  ## Return a data frame with the hospital names and the 
  ## (abbreviated) state name
  data.frame(hospital = unlist(rank), state = names(rank))
}