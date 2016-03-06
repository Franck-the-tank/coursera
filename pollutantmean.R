print("Don't forget to put directory and pollutant into \" ")

pollutantmean<- function(directory, pollutant, id= 1:332) {
  
  finalMean<-c()
  
  for (k in id){
    
        #Check the length of id in order to adapt the name of the path
        if (nchar(k)==1) {
          Id<-paste("00",k,sep = "")
        } else if(nchar(k)==2) {
          Id<-paste("0",k,sep = "")
        } else {
          Id<-toString(k)
        }
        
        path<-paste(paste(paste("coursera",directory,sep="/"), Id, sep = "/"), "csv", sep = ".")
        df <- read.csv(path)
        
        #put into a variable the data to calculate
        x<-df[pollutant]
        #remove the NAs from it
        x<-x[!is.na(x)]
        
        # Beware because doing the mean of several means is not getting the right result 
        #and you need to put all the data together before doing the mean
        finalMean<- append(finalMean, x)  
        
  }
  
  
  return(mean(finalMean))
  }