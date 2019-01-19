rankall <- function(outcome, num = "best") {
  
  ##read CSV
  outcomes <- read.csv("outcome-of-care-measures.csv")
  ##delete everything except state and outcomes
  keeps <- c("State","Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  y<-outcomes[keeps]
  
  z<-y$State
  s<-z[!duplicated(z)]
  s<-as.character(s)
  s<-s[order(s)]
  y[,2]<-as.character(y[,2])
  
  
  library(plyr)
  
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
  
    df<- data.frame("Hospital"=c(), "State"=c())
    
    for(j in 1:54){
        vector<- c()
        list<-c()
        list<-c(list, s[j])
        for(i in 1:4706){
          if(s[j] %in% arrangedData[i,1]){
            vector<-c(vector, arrangedData[i,2])
            
          }
        }
        if(num=="best"){
          list<-c(list, vector[1])
        } else if (num=="worst"){
          list<-c(list, tail(vector, n=1))
        } else{
          list<-c(list, vector[num])
        }
        df<-rbind(df, list)
        df[,1]<-as.character(df[,1])
        df[,2]<-as.character(df[,2])
    }
    df
}