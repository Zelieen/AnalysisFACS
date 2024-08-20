rm(list=ls())

# load libraries
# on first use also install libraries:
install.packages(c("tidyverse", "stringr", "scales")) #comment this line out on successive uses
library(tidyverse)
library(stringr)
library(scales)

# Fit function 

fitG =
  function(x, y, mu, sig, scale){
    f = function(p){
      d = p[3]*dnorm(x, mean = p[1], sd = p[2])
      sum((d-y)^2)
    }
    optim(c(mu, sig, scale), f)
  }

# Load data
CurrentPath <- readline(prompt = "Enter the path to Your .csv files: ") #something like C:\YourPathTo\SampleData\files_csv
files <- list.files(path = CurrentPath, pattern = "[^FitSpec]\\.csv") # "." is a regular expression for a wildcard single character, had to specify \\ before it to really look for a dot.
experimentDate <- as.numeric(readline(prompt = "Enter the date of the experiment as numbers only: ")) #for example 20240820 is the 20th August of 2024

# make first example plot for determining cutoff value

currentData<-read.csv(str_c(CurrentPath, "\\", files[1])) # specify the path of the file (used "\\" for Windows, Mac should be "/")
FSCtoSSC <- data.frame(currentData[, 1], currentData[, 4])
colnames(FSCtoSSC) <- c("FSC.A", "SSC.A")
plot(FSCtoSSC,pch = 20, 
     main=files[1]
     )

cutoff <- as.numeric(readline(prompt = "Enter value to gate above from: ")) #For the Sample Data enter 40000

abline(h = cutoff)

# Set up empty DataFrame for fit values

SampleSpecs <- data.frame(
  matrix(ncol = 7, nrow = 0),
  stringsAsFactors = FALSE)
colnames(SampleSpecs) <- c("Index", "FileName", "channel", "mu", "sigma", "scale", "date") 

dir.create(str_c(dirname(CurrentPath), "/", "files_png"))
ParentPath <- dirname(CurrentPath)

## start to cycle through loaded files

for (i in 1:length(files)){ 

  png(str_c(ParentPath, "/files_png/", i, "_", str_replace(files[i], "\\.csv", "_plots.png")), width = 480, height = 1200)
  par(mfcol = c(5,2),
      cex = 1.2,
      mai = c(0.6,0.6,0.6,0.6)
      )

# get data for current cycle

currentData<-read.csv(str_c(CurrentPath, "\\", files[i])) # specify the path of the file (used "\\" for Windows, Mac should be "/")

## Scatterplots
# SSC vs FSC

FSCtoSSC <- data.frame(currentData[, 1], currentData[, 4])
colnames(FSCtoSSC) <- c("FSC.A", "SSC.A")
plot(FSCtoSSC,
     pch = 20, 
     ylim = c(0, 200000), 
     main=files[i]
     )
abline(h = cutoff)

# SSC vs APC (far red)

APCtoSSC <- data.frame(currentData[, 7], currentData[, 4])
colnames(APCtoSSC) <- c("APC.A", "SSC.A")
plot(APCtoSSC,
     col = ifelse(APCtoSSC$SSC.A > cutoff,'red','black'),
     log = "x",
     pch = 20, 
     ylim = c(0, 200000),
     main=files[i]
     )
abline(h = cutoff)

# SSC vs DAPI (blue)

DAPItoSSC <- data.frame(currentData[, 8], currentData[, 4])
colnames(DAPItoSSC) <- c("DAPI.A", "SSC.A")
plot(DAPItoSSC,
     col = ifelse(DAPItoSSC$SSC.A > cutoff,'blue','black'),
     log = "x",
     pch = 20, 
     ylim = c(0, 200000),
     main=files[i]
)
abline(h = cutoff)

# SSC vs FITC (green)

FITCtoSSC <- data.frame(currentData[, 9], currentData[, 4])
colnames(FITCtoSSC) <- c("FITC.A", "SSC.A")
plot(FITCtoSSC,
     col = ifelse(FITCtoSSC$SSC.A > cutoff,'green','black'),
     log = "x",
     pch = 20, 
     ylim = c(0, 200000),
     main=files[i]
)
abline(h = cutoff)

# SSC vs PE (red)

PEtoSSC <- data.frame(currentData[, 10], currentData[, 4])
colnames(PEtoSSC) <- c("PE.A", "SSC.A")
plot(PEtoSSC,
     col = ifelse(PEtoSSC$SSC.A > cutoff,'orange','black'),
     log = "x",
     pch = 20, 
     ylim = c(0, 200000),
     main=files[i]
)
abline(h = cutoff)

## Histograms
# reduce data

relevantData <- data.frame(currentData[, 1], currentData[, 4], currentData[,7], currentData[,8], currentData[,9], currentData[,10])
colnames(relevantData) <- c("FSC.A", "SSC.A", "APC.A", "DAPI.A", "FITC.A", "PE.A")
GatedData <- relevantData[relevantData$SSC.A > cutoff,]

# FSC - Actually not necessary, only for even number of plots

hist(GatedData[,"FSC.A"], 
     breaks = seq(0, 100 + round(max(GatedData[,"FSC.A"]), digits = 0), 100),
     xlab = "FSC.A",
     main = files[i],
     lty= "blank",
     border = NULL,
     col = "black"
     )

ChannelsName <- c("APC.A", "DAPI.A", "FITC.A", "PE.A")
ChannelsColor <- c("red", "blue", "green", "orange")

## start to cycle through color channels

for (j in 1:length(ChannelsName)){ 

# make logarithmic Data of channel

logGated <- log10(GatedData[,ChannelsName[j]])
logGated <- logGated[!is.nan(logGated)]
logGated <- logGated[!is.na(logGated)]



## Calculate Fit

hist_x <- hist(logGated, breaks = seq(0, 5.5, 0.0125),plot=FALSE)$breaks
hist_y <- hist(logGated, breaks = seq(0, 5.5, 0.0125),plot=FALSE)$counts

fitData <- fitG(hist_x[1:length(hist_y)],
                hist_y,
                6.5,##change here for best peak approximation to ensure good fit
                1.1,
                50)
p = fitData$par

## Save Fit to DataFrame

SampleSpecs <- rbind(SampleSpecs, c(i, files[i], ChannelsName[j], p[1], p[2], p[3], experimentDate), stringsAsFactors = FALSE)
colnames(SampleSpecs) <- c("Index", "FileName", "channel", "mu", "sigma", "scale", "date") 

## make histogram with fit curve

hist(logGated, xlim = c(0, 5.5), 
     breaks = seq(0, 0.1 + round(max(logGated), digits = 1), 0.0125),
     xlab = str_c(ChannelsName[j], " intensity as 10^x"),
     ylab = "Count",
     main = ChannelsName[j],
     lty= "blank",
     border = NULL,
     col = "black"
     )
lines(hist_x[1:length(hist_y)],
      p[3]*dnorm(hist_x[1:length(hist_y)], p[1], p[2]),
      col = ChannelsColor[j],
      lwd = 2,
      )
legend("topleft",
       cex = 0.75,
       legend = c(expression(paste(mu, " = ")), round(fitData$par[1],3), 
         c(expression(paste(sigma," = ")), round((fitData$par[2]), 3)))
      )

print(ChannelsName[j])
} #go up to next color channel

dev.off() # shuts off png writing

# end cycle

print(files[i])
} #go up and to next file in the cycle and repeat

# save fit data

write.csv(SampleSpecs, file = str_c(CurrentPath, "\\FitSpec.csv"), row.names = FALSE)
print(str_c(i, " files were processed"))