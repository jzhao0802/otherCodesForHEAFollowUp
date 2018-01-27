rm(list=ls())

library(ggplot2)

addXMinMax <- function(x, label="A"){
  n <- length(x) - 1
  
  data.frame(x=seq(from=min(x), to=max(x), by=((max(x)-min(x))/n)), y=x, label=label)
}

raggedListToDf <- function(x, labels=LETTERS[seq_along(x)]){
  do.call(rbind, 
          lapply(seq_along(x), 
                 function(i) addXMinMax(x[[i]], label=labels[i])))
}

PlotDensities <- function(dataDir)
{
  predsEval <- read.csv(paste(dataDir, "predsPosData.csv", sep=""), 
                        header=F, sep=",")
  
  predsNew <- read.csv(paste(dataDir, "predsNegData.csv", sep=""), 
                       header=F, sep=",")
  
  plotData <- raggedListToDf(list(predsEval=predsEval[,1],
                                  predsNew=predsNew[,1]), 
                             labels=c("Positive", "Negative"))
  
  p<-ggplot(plotData, aes(y, group=label)) + 
    geom_density(aes(group=label, colour=label))
  # p<-ggplot(plotData, aes(x, y, label=label, group=label)) + geom_text()
  
  print(p)
  
  timeStamp <- as.character(Sys.time())
  timeStamp <- gsub(":", ".", timeStamp)  # replace ":" by "."
  resultDir <- paste("./Results/", timeStamp, "/", sep = '')
  dir.create(resultDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")
  
  fileDataDir <- file(paste(resultDir, "dataDir.txt", sep=""), "w")
  writeLines(dataDir, fileDataDir)
  close(fileDataDir)
  
  ggsave(paste(resultDir, "density_decision_values.png", sep=""),
         width=4,height=3)
}

