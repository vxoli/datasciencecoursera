rankall <- function(outcome, num = "best", data="data/outcome-of-care-measures.csv"){
  ## read data from data file
  stateData <- read.csv(data, colClasses = "character")
  ## check state and outcome entry valid before proceeding. If not exit with error message and return NA.
  if (!(outcome %in% c("pneumonia", "heart attack", "heart failure"))) {
    print("Input bad state or outcome")
    return(NA)
  }
  ## determine lowest value for outcome in state depending on outcome parameter
  ## "heart attack" -> col 11
  ## "heart failure" -> col 17
  ## "pneumonia" -> col 23
  ## colIndex is a var set to the column number according to the outcome requested
  colIndex <- (outcome == "heart attack") * 11 + (outcome == "heart failure") * 17 + (outcome == "pneumonia") * 23
  ## get listof unique states
  uniqueStates <- sort(unique(stateData$State))
  ## convert outcome col to numeric
  stateData[,colIndex] <- as.numeric(stateData[,colIndex])
  result.df <- list()
  
  for(state in uniqueStates) {
    data.state <- stateData[stateData$State == state, ]
    data.state[, colIndex] <- as.numeric(x=data.state[, colIndex])
    data.state <- data.state[complete.cases(data.state), ]
  ## if num == best set num <-1
  ## if num == worst set num <- last entry with a value (ie not NA)
  if (num == "best") {
    numrank <- 1
    }
    else if (num == "worst") {
      numrank <- nrow(data.state)
    } else {
      numrank <- num
    }

  data.state <- data.state[order(data.state[,colIndex], data.state$Hospital.Name), ]
  return.names <- data.state[numrank, ]$Hospital.Name
  result.df <- rbind(result.df, list(return.names[1], state))
  
  
  }

  result.df <- as.data.frame(x=result.df)
  colnames(x=result.df) <- c('hospital', 'state')
  result.df
}