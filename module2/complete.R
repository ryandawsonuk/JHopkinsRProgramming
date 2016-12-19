complete <- function(directory, id=1:332){
  
  #read all the filenames, unfiltered at this point
  allfiles <- list.files(path=directory, pattern="*.csv")
  
  #need a vector to store the counts in
  nobs <- vector()
  
  #for loop to read files recursively from directory
  #but only read files with names corresponding to ids
  for(i in id){
    #pad with leading zeros
    filename <- sprintf("%03d.csv", i)
    
    filenamepath = paste(directory,filename,sep="/")
    frame <- read.csv(filenamepath)
    
    #filter to complete cases
    completeCases <- frame[complete.cases(frame),]
    #count them
    numcompletecases <- nrow(completeCases)
    #record the count as a new element in the vector
    nobs <- c(nobs,numcompletecases)
  }
  
  #create data frame from the vectors
  data.frame(id=id, nobs=nobs)
}