rankhospital <- function(state, outcome, num, data="data/outcome-of-care-measures.csv"){
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
  ## convert outcome col to numeric
  stateData[,colIndex] <- as.numeric(stateData[,colIndex])
  ## order the data frame by metric then hospital name
  rankedHospital <- stateData[order(stateData[,colIndex], stateData[,2], decreasing = FALSE, na.last=TRUE),]
  ## return the name in the num'th row of rankedHospital
  ## if num == best set num <-1
  ## if num == worst set num <- last entry with a value (ie not NA)
  if (num == "best") {num <- 1}
  if (num == "worst") {num <- length(na.omit(rankedHospital[,colIndex])) }
  return(rankedHospital[num,2])
}