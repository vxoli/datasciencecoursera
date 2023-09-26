best <- function(state, outcome, data="outcome-of-care-measures.csv") {
  ## read data from data file
  dataset <- read.csv(data)
  ## check state and outcome entry valid before proceeding. If not exit with error message and return NA.
  if (!((state %in% dataset$State) & (outcome %in% c("pneumonia", "heart attack", "heart failure")))) {
    print("Input bad state or outcome")
    return(NA)
  }
  ## split data by state
  stateData <- as.data.frame(split(dataset, dataset$State == state)["TRUE"])
  
  ## determine lowest value for outcome in state depending on outcome parameter
  ## "heart attack" -> col 11
  ## "heart failure" -> col 17
  ## "pneumonia" -> col 23
  ## colIndex is a var set to the column number according to the outcome requested
  colIndex <- (outcome == "heart attack") * 11 + (outcome == "heart failure") * 17 + (outcome == "pneumonia") * 23
  stateData[,colIndex] <- as.numeric(stateData[,colIndex])
  ## now find the min of the column and return name of the best hospital from col 2
  bestHosp <- stateData[which(stateData[,colIndex] == min(stateData[,colIndex],na.rm=TRUE)),2]
  return(bestHosp)
}