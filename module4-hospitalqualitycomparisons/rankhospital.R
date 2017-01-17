rankhospital <- function(state, outcome, num="best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character");
    
    ## Check that state and outcome are valid
    # valid outcomes are  "heart attack", "heart failure", or "pneumonia", otherwise stop with "invalid outcome"
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
    
    # If an invalid state value is passed to best,  the function should throw an error via the
    # stop function with the exact message "invalid state"
    validStates <- unique(data[,7]);
    stateMatch <- match(x=tolower(state), table=tolower(validStates),nomatch = 0);
    if(stateMatch == 0){
        stop("invalid state");
    }
    
    #need to know columns that contain 30-day rates for each outcome
    #11 for heart attack, 17 for heart failure and, 23 for pneumonia
    #note in same order as validOutcomes so this can be used to match
    outcomeCols <- c(11, 17, 23);
    
    #find which column for this outcome
    outcomeColMatch <- outcomeCols[outcomeMatch];
    
    #filter data by state and just keep name and relevant outcome column
    outcomeData <- data[data[,7]==toupper(state),c(2,outcomeColMatch)];
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate for that outcome
    #  Hospitals that do not have data on a particular
    #outcome should be excluded from the set of hospitals when deciding the rankings
    
    
    #take names (first col) for which outcome (second col) is minimum
    #make numeric
    outcomeData[, 2] = suppressWarnings( as.numeric(outcomeData[, 2]) )
    #exclude nas
    outcomeData = na.omit(outcomeData);
    
    #If there is a tie for the best hospital for a given outcome, then the hospital names should
    #be sorted in alphabetical order and the first hospital in that set should be chosen
    #so order by outcome and then hospital name
    outcomeData = outcomeData[order(outcomeData[,2], outcomeData[,1]),];
    
    if(num=="best"){
        return (outcomeData[1,1]);
    }
    if(num=="worst"){
        return (outcomeData[nrow((outcomeData)),1]);
    }
    return (outcomeData[num,1]);
}