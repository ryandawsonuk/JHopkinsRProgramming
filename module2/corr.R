corr <- function(directory, threshold=0){
  nobsFrame <- complete(directory)
  nobsFiltered <- subset(nobsFrame, nobsFrame$nobs > threshold)
  
  correlations <- vector('numeric')
  
  for(i in nobsFiltered$id){
    
    #pad with leading zeros
    filename <- sprintf("%03d.csv", i)
    
    filenamepath = paste(directory,filename,sep="/")
    frame <- read.csv(filenamepath)
    
    #filter to complete cases
    completeCases <- frame[complete.cases(frame),]

    correlations <- c(correlations, cor(completeCases$nitrate, completeCases$sulfate) )
  }
  
  correlations
}