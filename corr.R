corr <- function(directory, threshold=0) {
  
  df <- complete(directory)
  df1 <- df[df$nobs > threshold,]
  correlationVec = NULL
  
  if (nrow(df1) != 0)
  {
    for(i in 1:nrow(df1)) {
      data_i <- readData(df1[i,1], directory)
      cleanData_indicator <- apply(data_i, 1, isNotNa)
      cleanData_i <- data_i[cleanData_indicator,]
      
      correlationVec <- c(correlationVec, cor(cleanData_i$nitrate, cleanData_i$sulfate))
    }
  }
  
  correlationVec
}

complete <- function(directory, id =1:332) {
  nobs = NULL
  
  for (i in id ) {
    data_i <- readData(i, directory)
    nobs_i = sum(apply(data_i, 1, isNotNa))
    nobs = c(nobs, nobs_i)
  }
  
  data.frame(id, nobs)
}

isNotNa <- function(row) {
  !is.na(row[2]) && !is.na(row[3])
}

readData <- function(i, directory) {
  
  fileName = NULL
  
  if ( i < 10) {
    fileName = paste("00", i, ".csv", sep='')
  }
  else if ( i < 100) {
    fileName = paste("0", i, ".csv", sep='')
  }
  else {
    fileName = paste(i, ".csv", sep='')
  }
  
  filePath = paste(getwd(), directory, fileName, sep='/')
  data_i <- read.csv(file=filePath, sep=",")
}

