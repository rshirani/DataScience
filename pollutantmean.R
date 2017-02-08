pollutantmean <- function(directory, pollutant, id = 1:332) {

  data = NULL
  data_pollutant = NULL
  
  for (i in id ) {
    if ( i < 10) {
      fileName = paste("00", i, ".csv", sep='');
    }
    else if ( i < 100) {
      fileName = paste("0", i, ".csv", sep='');
    }
    else {
      fileName = paste(i, ".csv", sep='')
    }
    
    filePath = paste(getwd(), directory, fileName, sep='/')
    data_i <- read.csv(file=filePath, sep=",")
    data <- rbind(data, data_i)
  }
  
  if(pollutant == "sulfate"){
    data_pollutant = data$sulfate
  }
  else if (pollutant == "nitrate") {
    data_pollutant = data$nitrate
  }
  else{
    print("Error")
  }
  
  mean(data_pollutant, na.rm = TRUE)

}



