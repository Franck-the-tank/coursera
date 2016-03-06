#function that reads a directory full of files and reports 
#the number of completely observed cases in each data file
test<-"program loaded"
complete<- function(directory,id=1:332){

  final <- data.frame("id"=integer(), "nobs"=integer())
  # Need to make it work for several dataframes
  for (k in 1:length(id)){
   
       #Check the length of id in order to adapt the name of the path
      if (nchar(id[k])==1) {
        Id<-paste("00",id[k],sep = "")
      } else if(nchar(id[k])==2) {
        Id<-paste("0",id[k],sep = "")
      } else {
        Id<-toString(id[k])
      }
      
      #set the path to in order to define the data frame
      path<-paste(paste(paste("coursera",directory,sep="/"), Id, sep = "/"), "csv", sep = ".")
      dataframe <- read.csv(path)
      
      #return a data frame where the first column is the name of the file
      #and the second column is the number of complete cases  
      
      #complete cases alloww to check if cases are complete or not
      #In that case we just keep the part of the dataframe that are not empty 
      #(check on both interesting columns)
      
      transi<-dataframe[complete.cases(dataframe[2]),]
      transi2 <- transi[complete.cases(transi[3]),]
      
      final[nrow(final) + 1, ] <- c( id[k], nrow(transi2))
      
      
    }
          
            
  print(final)

  
  
}