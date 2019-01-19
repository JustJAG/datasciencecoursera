rankhospital <- function(state, outcome, num = "best") {
  
  ##read CSV
  outcomes <- read.csv("outcome-of-care-measures.csv")
  ##delete everything except state and outcomes
  keeps <- c("State","Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  y<-outcomes[keeps]
  
  y[,2]<-as.character(y[,2])
  
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
    vector <- c()
    for(i in 1:4706){
      if(state %in% arrangedData[i,1]){
        vector<-c(vector, arrangedData[i,2])
      
      }
    }
    if(num=="best"){
      print(vector[1])
    } else if (num=="worst"){
      tail(vector, n=1)
    } else{
      print(vector[num])
    }
    
  } else{
    "Please enter a correct state abbr"
  }
}