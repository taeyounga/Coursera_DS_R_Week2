rm(list=ls()) #Clear memory

setwd("D:/Coursera") #Set Workind directory

pollutantmean <- function(directory, pollutant, id=1:332){ #As given by the question
        pol_vect <- c() #Create an empty vector to store pollutant data
        all_files <- as.character(list.files(directory)) #Create list of all files
        file_paths <- paste(directory,all_files,sep="/") #Create a list of files with full path (directory/filename)
        for(i in id){
                pol_vect <- append(pol_vect, read.csv(file_paths[i], header = TRUE)[,pollutant])
                #use append to keep adding the pollutant data to the pol_vect. the [,pollutant]'s comma is there to make pol_vect a numeric vector instead of dataframe
        }
        mean(pol_vect, na.rm=TRUE)
}