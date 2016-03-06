#Calculation of the correlation between nitrate and sulfate data

corr<- function(directory, threshold = 0){
  
  final <- c()
  #make the entire dataframe with id and nobs
  dfCases <- complete(directory)
  
  #compare the threshold to the value of nobs
  # it returns only a dataframe with the number of complete cases over threshold
  dfThreshold <- dfCases[dfCases$nobs>threshold,]
  
  if (nrow(dfThreshold)>0) {
 

  
        for (k in 1:nrow(dfThreshold)){
              
              fileId <- dfThreshold[k,1]
            
              #Check the length of id in order to adapt the name of the path
              if (nchar(fileId)==1) {
                Id<-paste("00",fileId,sep = "")
              } else if(nchar(fileId)==2) {
                Id<-paste("0",fileId,sep = "")
              } else {
                Id<-toString(fileId)
              }
              #set the path to in order to define the data frame
              path<-paste(paste(paste("coursera",directory,sep="/"), Id, sep = "/"), "csv", sep = ".")
              fileToDo<- read.csv(path)
              
              #take only the complete lines
              fileCompleteTransi<- fileToDo[complete.cases(fileToDo[2]),]
              fileComplete <- fileCompleteTransi[complete.cases(fileCompleteTransi[3]),]
              
              #Calculate the correlation of these
              calculation <- cor(fileComplete[2], fileComplete[3])
              
              #make the final vector for all correlations
              final<- append(final, calculation)
        
          }
  } else {
    
    final<-c()
  }

  # Return a vector of correlation for the monitors that meet the threshold requirements
  # If none meets the threshold han just have a vector of length 0
  print(final)  
  
}