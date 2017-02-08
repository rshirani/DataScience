complete <- function(directory, id =1:332) {
  nobs = NULL
  
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
    
    nobs_i = sum(apply(data_i, 1, isNotNa))
    nobs = c(nobs, nobs_i)
    
  }
  
  data.frame(id, nobs)
}

isNotNa <- function(row) {
  !is.na(row[2]) && !is.na(row[3])
}

