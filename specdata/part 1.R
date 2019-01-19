pollutantmean<- function(directory, pollutant, id=1:332){
    files <- list.files(directory,full.names=TRUE)
    #empty data frame
    dat <- data.frame()
    #bind them all
    for (i in id){
        dat <- rbind(dat, read.csv(files[i]))
    }
    #calculate mean
    if (pollutant=="nitrate"){
        n<- mean(dat[,3], na.rm=TRUE)
    } else{
        n<-mean(dat[,2], na.rm=TRUE)
    }
    print(n)
}

pollutantmean("specdata", "sulfate", 1:10)
