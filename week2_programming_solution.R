rm(list=ls()) #Clear memory

setwd("D:/Coursera") #Set Working directory

##PART1##
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

############Demo comparison##################
# > pollutantmean("specdata","sulfate",1:10)
# [1] 4.064128 - CORRECT
# > pollutantmean("specdata","nitrate",70:72)
# [1] 1.706047 - CORRECT
# > pollutantmean("specdata","nitrate",23)
# [1] 1.280833 - CORRECT
#############################################

##PART2##
complete <- function(directory, id=1:332){
        all_files <- as.character(list.files(directory)) #Create list of all files
        file_paths <- paste(directory,all_files,sep="/") #Create a list of files with full path (directory/filename)
        #result <- data.frame(id = integer(), nobs = integer())
        datalist <- list()
        j <- 1
        for(i in id){
                comp <- read.csv(file_paths[i])
                comp_row <- nrow(comp[complete.cases(comp), ])
                datalist[[j]] <- comp_row
                j <- j+1
        }
        #result = do.call(rbind, datalist)
        result <- as.data.frame(cbind(id,nobs = datalist))
        return(result)
}

complete("specdata",2:3)


corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        
        # get the complete table
        complete_table <- complete("specdata", 1:332)
        nobs <- complete_table$nobs
        # find the valid ids
        ids <- complete_table$id[nobs > threshold]
        # get the length of ids vector
        id_len <- length(ids)
        corr_vector <- rep(0, id_len)
        # find all files in the specdata folder
        all_files <- as.character( list.files(directory) )
        file_paths <- paste(directory, all_files, sep="/")
        j <- 1
        for(i in ids) {
                current_file <- read.csv(file_paths[i], header=T, sep=",")
                corr_vector[j] <- cor(current_file$sulfate, current_file$nitrate, use="complete.obs")
                j <- j + 1
        }
        result <- corr_vector
        return(result)   
}
