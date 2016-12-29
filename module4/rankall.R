rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character");
    
    ## Check that num and outcome are valid
    validOutcomes <- c("heart attack", "heart failure", "pneumonia");
    outcomeMatch <- match(x=tolower(outcome), table=validOutcomes,nomatch = 0);
    if(outcomeMatch == 0){
        stop("invalid outcome");
    }
    
    #check the num parameter
    if(match(x=tolower(num), table=c("best","worst"),nomatch = 0)==0){
        if(is.na(as.numeric(num))){
            stop("invalid num");
        }
    }
    
    #need to know columns that contain 30-day rates for each outcome
    #11 for heart attack, 17 for heart failure and, 23 for pneumonia
    #note in same order as validOutcomes so this can be used to match
    outcomeCols <- c(11, 17, 23);
    
    #find which column for this outcome
    outcomeColMatch <- outcomeCols[outcomeMatch];
    
    #make outcome numeric
    data[, outcomeColMatch] = suppressWarnings( as.numeric(data[, outcomeColMatch]) );
    
    # Remove rows with NA
    data = data[!is.na(data[,outcomeColMatch]),]
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    #split data by state (which is 7th col)
    splitdata = split(data, data[,7]);
    
    allranked = lapply(splitdata, function(outcomeData, num) {

        
        # order by outcome and then hospital name
        outcomeData = outcomeData[order(outcomeData[,outcomeColMatch], outcomeData[,1]),];
        
        
        #return row for appropriate ranked hospital
        #col 2 is hosp name
        if(num=="best"){
            return (outcomeData[1,2]);
        }
        if(num=="worst"){
            return (outcomeData[nrow((outcomeData)),2]);
        }
        return (outcomeData[num,2]);
    }, num);

    #data is now in a list of variables with name state and val hosp name
    #so unlist to get the data out and into a frame with the names as another col
    return ( data.frame(hospital=unlist(allranked), state=names(allranked)) );
}