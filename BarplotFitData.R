#rm(list=ls())

# load libraries
#install.packages(c("tidyverse", "stringr", "scales", "ggplot2")) #comment this line out on successive uses
library(tidyverse)
library(stringr)
library(scales)
library(ggplot2)

# Load Fit data

#CurrentPath <- readline(prompt = "Enter the path to Your FitSpec.csv files: ") #something like C:\YourPathTo\SampleData\files_csv
files <- list.files(path = CurrentPath, pattern = "FitSpec*\\.csv")

# read just first Data file

#CurrentData<-read.csv(str_c(CurrentPath, "\\", files[1]))
#FitData <- CurrentData

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

# reorder files so that the order is pos/ neg/ unmod and then colors will be the same
# will work for multiple pos/ neg/ unmod as well
PosCon <- c()
Blank <- c()
NegCon <- c()
OtherSamples <- c()

for (i in 1:length(FitData$FileName)){
  if (grepl("pos|PC|max",FitData[i,"FileName"])==TRUE){
    PosCon<-rbind(PosCon, FitData[i,])
  }
  else if (grepl("neg|Blank|blank",FitData[i,"FileName"])==TRUE){
    Blank<-rbind(Blank, FitData[i,])
  }
  else if (grepl("unmod|NC|min",FitData[i,"FileName"])==TRUE){
    NegCon<-rbind(NegCon, FitData[i,])
  }else{
    OtherSamples<-rbind(OtherSamples, FitData[i,])
  }
}

FitData <- rbind(PosCon, Blank, NegCon, OtherSamples)

ParentPath <- dirname(CurrentPath)

##prepare Data to be plotted
BarData <- FitData$FileName
BarData <- as.data.frame(BarData)
BarData <- cbind(BarData, FitData$channel)
BarData <- cbind(BarData, as.numeric(FitData$mu))
BarData <- cbind(BarData, as.numeric(FitData$sigma))
colnames(BarData) <- c('FileName', 'channel', 'mu', 'sigma')
#extract only APC.A channel rows
BarData <- BarData[BarData$channel == "APC.A", ]

ggplot(BarData) +
  ggtitle("Fluorescence peaks") +
  geom_bar( aes(x=FileName, y=mu), stat="identity", fill="grey", width = 0.6) +
  geom_errorbar( aes(x=FileName, ymin=mu-sigma, ymax=mu+sigma), width=0.3, colour="black", alpha=0.9, size=1) +
  geom_text(label = round(BarData$mu, digits = 2), x = 1:length(BarData$FileName), y = 1, angle = 90) +
  labs(x = "Sample") +
  scale_y_continuous(expand = c(0,0), breaks = 1:5, limits = c(0, 6)) + #margin, ticks, limits
  scale_x_discrete(labels = str_sub(BarData$FileName, end = -5), limits=BarData$FileName) + #use existing order of dataframe
  theme(text=element_text(size=20),
        plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = 'white'),
        panel.grid.major.y = element_line(color = 'grey'),
        panel.grid.minor = element_line(),
        axis.line.y = element_line(colour = 'black', size=1, linetype='solid'),
        axis.line.x = element_line(colour = 'black', size=1, linetype='solid'), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t=10))
        )
ggsave(str_c(ParentPath, "/", "Combined_Barplot.png"), width = 600, height = 600, units = "px", scale = 4)

print(BarData$FileName)
