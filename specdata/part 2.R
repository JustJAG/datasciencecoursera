complete<- function(directory, id=1:332){
    files <- list.files(directory,full.names=TRUE)
    #empty data frame
    dat <- data.frame()
    
    #bind them all
    for (i in id){
        x<-read.csv(files[i])
        s<-complete.cases(x)
        a<-sum(s)
        dat <- rbind(dat, c(i, a))
    }
    colnames(x, do.NULL = TRUE, prefix = "col")
    colnames(dat) <- c("id","nObs")
    dat
}

