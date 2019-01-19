corr<- function(directory, threshold=0){
    files <- list.files(directory,full.names=TRUE)
    #empty data frame
    dat <- data.frame()    
    for (i in 1:332){
        x<-read.csv(files[i])
        s<-complete.cases(x)
        a<-sum(s)
        if(a>threshold) {
            c<-cor(x$nitrate, x$sulfate, use="complete.obs")
            dat <- rbind(dat, c)
        }
        
    }

    dat
}

