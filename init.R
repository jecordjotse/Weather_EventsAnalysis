#library(dplyr);library(reshape2);library(ggplot2)
## function to calculate memory of dataset
memoryReq <- function(rows, cols){
  RAM <- (as.numeric(rows) * as.numeric(cols) *8)/ 2^20 # in MB
  return(list(MB=round(RAM,2),GB=round(RAM/2^10,2)))
}



##function to read and clean file
readFileInto <- function(){
  if(!file.exists('./data/repdata_data_StormData.csv.bz2')){
      download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2',destfile = './repdata_data_StormData.csv.bz2')
  }
  dataSet <<- read.csv('./data/repdata_data_StormData.csv.bz2',header = TRUE, sep = ",", na.strings = "?")
}

# function to save plots
savePNG <- function(fileName){
  ## Saving to PNG
  dev.copy(png, file=fileName, height=480, width=480)
  dev.off()
}

validateFileSize <- function(){
computerRAM = 8
if(memoryReq(902297,37)$GB>computerRAM/10){
  stop("File too large",call. = TRUE)
}
}
loadData <- function(){
if(!exists("dataSet")){
  readFileInto()
}else if(nrow(dataSet)>3000&ncol(dataSet)!=8){
  readFileInto()
}
}
