# Create a directory where you can create a lot of files
# e.g. AdvancedR\Session1
dir.create("session1/")
setwd("session1/")
getwd()
# create several files for later read in
for (i in 1:100)
write.csv(file=paste("file_",i,".csv",sep=""),data.frame(y=rnorm(20,0,3)))

#Exercise 1
#write a function to load and retrieve the mean and max of each file
#plot a boxplot
library(strengejacke)
library(tibble)
library(tidyverse)
folder <- getwd()
files <- dir(path = getwd())

loadAndDescriptive <- function(files){
  data <- c()
  means <- c()
  maxs <- c()
  for(i in 1:length(files)){
   d <- read.csv(files[i])
   data <- c(data, d)
   meany <- mean(d$y)
   maxy <- max(d$y)
   means <- c(means, meany)
   maxs <- c(maxs, maxy)
  }
  return(data_frame(means, maxs))
}

data <- loadAndDescriptive(files)

plotdata <- function(k){
  boxplot(k)
  print(mean(k$y))
}


#plot the first 10 datasets
par(mfrow=c(2,5)) #allows plotting 2 by 5 plots at once




#now plot only extreme datasets
#where the mean is more than 1 away from 0
#give the number of the data set
#write a separate function for this




#let's hope it's not more than 16:)
dev.off() #deletes the currently active plot
par(mfrow=c(4,4)) 


#identify all data files with the pattern="file*"
#and delete them



# if you are finished early read the chapter on style guide
# https://swcarpentry.github.io/r-novice-inflammation/06-best-practices-R/
# or
# http://adv-r.had.co.nz/Style.html
# or
# https://google.github.io/styleguide/Rguide.xml
# report this to other participants later

