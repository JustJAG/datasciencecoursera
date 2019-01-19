best <- function(state, outcome) {
    ##read CSV
    outcomes <- read.csv("outcome-of-care-measures.csv")
    ##delete everything except state and outcomes
    keeps <- c("State","Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
    y<-outcomes[keeps]
    library(plyr)
    if(state %in% y[,1]){
        if(outcome %in% "heart attack"){
          y[,3] <- as.numeric(as.character(y[,3]))
          y <- y[complete.cases(y[,3]), ]
          arrangedData <- arrange(y, y[,3], y[,2])
        } else if (outcome %in% "heart failure"){
          y[,4] <- as.numeric(as.character(y[,4]))
          y <- y[complete.cases(y[,4]), ]
          arrangedData <- arrange(y,y[,4], y[,2])
        } else if (outcome %in% "pneumonia") {
          y[,5] <- as.numeric(as.character(y[,5]))
          y <- y[complete.cases(y[,5]), ]
          arrangedData <- arrange(y,y[,5], y[,2])
        } else {
          print("Please enter a correct disease name")
        }
      
        for(i in 1:4706){
          if(state %in% arrangedData[i,1]){
            print(arrangedData[i,2])
            break
          }
        }
      
    } else{
      "Please enter a correct state abbr"
    }
    
    
    
    
    ## Check that state and outcome are valid
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
}

