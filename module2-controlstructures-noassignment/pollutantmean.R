pollutantmean <- function(directory, pollutant, id = 1:332){
  
  #read all the filenames, unfiltered at this point
  allfiles <- list.files(path=directory, pattern="*.csv")
  
  #vector to for the values we want
  allvals <- vector();
  
  #for loop to read files recursively from directory
  #but only read files with names corresponding to ids
  for(i in id){
    #pad with leading zeros
    filename <- sprintf("%03d.csv", i)

    filenamepath = paste(directory,filename,sep="/")
    frame <- read.csv(filenamepath)
    
    
    #extract values for that pollutant
    pollutantcol <- frame[pollutant]
    
    #remove NAs
    pollutantcol <- pollutantcol[!is.na(pollutantcol)]
    
    allvals <- c(allvals,pollutantcol)
    
  }

  mean <- mean(allvals)
  round(mean,3)
}