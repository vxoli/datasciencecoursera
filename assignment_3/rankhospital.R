rankhospital <- function(state, outcome, num, data="outcome-of-care-measures.csv"){
  ## read data from data file
  dataset <- read.csv(data)
  ## check state and outcome entry valid before proceeding. If not exit with error message and return NA.
  if (!((state %in% dataset$State) & (outcome %in% c("pneumonia", "heart attack", "heart failure")))) {
    print("Input bad state or outcome")
    return(NA)
  }
  ## split data by state
  stateData <- as.data.frame(split(dataset, dataset$State == state)["TRUE"])
  
  
}