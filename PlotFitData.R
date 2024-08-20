rm(list=ls())

# load libraries
library(tidyverse)
library(stringr)
library(scales)

# Load Fit data

CurrentPath <- readline(prompt = "Enter the path to Your FitSpec.csv files: ")
files <- list.files(path = CurrentPath, pattern = "FitSpec*\\.csv")

# read just first Data file (optional)

#CurrentData<-read.csv(str_c(CurrentPath, "\\", files[1]))
#FitData <- CurrentData
#FitData <- read.csv("C:\\Users\\fkerlin\\Videos\\_Spectral\\FACS\\Fiona_20230126\\files_csv\\FitSpec.csv")

# or concatenate all Data

for (i in 1:length(files)){ 
  CurrentData<-read.csv(str_c(CurrentPath, "\\", files[i]))
  if (i==1){
    FitData <- CurrentData
  }else{
    FitData <- rbind(FitData, CurrentData)
  }
}

# alphanumerical sort FitData
FitData <- FitData[order(FitData$FileName), ]
FitData <- FitData[FitData$channel == "APC.A",] #restrict plotting to the APC.A channel

# reorder files so that the order is pos/ neg/ unmod and then colors will be the same
# will work for multiple pos/ neg/ unmod as well
PosCon <- c()
Blank <- c()
NegCon <- c()
OtherSamples <- c()

for (i in 1:length(FitData$FileName)){
  if (grepl("pos|PC",FitData[i,"FileName"])==TRUE){
    PosCon<-rbind(PosCon, FitData[i,])
  }
  else if (grepl("neg|Blank|blank",FitData[i,"FileName"])==TRUE){
    Blank<-rbind(Blank, FitData[i,])
  }
  else if (grepl("unmod|NC",FitData[i,"FileName"])==TRUE){
    NegCon<-rbind(NegCon, FitData[i,])
  }else{
    OtherSamples<-rbind(OtherSamples, FitData[i,])
  }
}

FitData <- rbind(PosCon, Blank, NegCon, OtherSamples)

# plotting histograms with lines

#specify the vector of colors to use
LineCols <- #brewer.pal(8, "Set1")
  ##or define by hand:
  c("red", "grey", "black", "olivedrab3", "green", "cyan", "steelblue2", "mediumblue", "darkviolet", "magenta2", "indianred3", "orange", "lightgoldenrod2", "yellow3", "lemonchiffon3", "darkseagreen", "lightblue")
##visualize your defined color vector
#plot(1:length(LineCols), 1:length(LineCols), col=LineCols[1:length(LineCols)], pch=19, cex=3, xlab="", ylab="")
palette(LineCols)

ParentPath <- dirname(CurrentPath)
png(str_c(ParentPath, "/", "Combined_Curves.png"), width = 600, height = 600, units = "px", pointsize = 12
    )

#first plot an empty histogram for the curves
hist(0,
     breaks = seq(0, 5.5, 0.0125),
     xlab = "Fluorescence in log10",
     ylab = "Frequency",
     ylim = c(0,80),
     xlim = c(0, 5.5),
     main = "Bead Fluorescence",
     col= rgb(0.3,0.3,0.3,1/1000),
     lty= "blank",
     border = NULL,
     col.axis = rgb(1,1,1,0),
)
axis(1)

# add fitted curves

for (i in 1:length(FitData$FileName)){  
    lines(seq(0, 5.5, 0.0125),
          14*dnorm(seq(0, 5.5, 0.0125), FitData[i,"mu"], FitData[i,"sigma"]),
          col = i,
          lwd = 2,
    )
}

legend("topleft", legend = FitData[,"FileName"], fill = 1:length(FitData$FileName))


dev.off() # shuts off png writing

print(FitData$FileName)
