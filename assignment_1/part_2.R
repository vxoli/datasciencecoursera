pollutantmean <- function(directory = "specdata", pollutant, id = 1:322) {
  ## 'directory' is a character vector of length 1 indicating the location for the CSV files
  ## 'pollutant' is a character vector of length 1 indicating the name of the pollutant for which we will calculate the mean; either "sulfate" or "nitrate".
  ## 'id' is an integer vector indicating the monitor ID numbers to be used
  ## Return the mean of the pollutant across all monitors listed in the 'id' vector. Ignore NA values.
  ## Do NOT round the result.
  
  files <- list.files(paste("~/Documents/Coursera_Data_Science/datasciencecoursera/assignment_1/",directory, sep=""))[id]
  readfiles <- lapply(files, read.csv)
  data <- do.call(rbind, readfiles)
  mean(data[,pollutant], na.rm = TRUE)

}

complete <- function(directory = "specdata", id = 1:332) {
  files <- list.files(paste("~/Documents/Coursera_Data_Science/datasciencecoursera/assignment_1/",directory, sep=""))[id]
  counter <- 1
  output <- c()
  for (file in files) {
    data <- na.omit(read.csv(file))
    output[counter] <- nrow(data)
    counter <- counter + 1
  }
  output
}