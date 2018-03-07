setwd("C:/Users/stephen.l.kingery/Desktop/Coursera/rprog_data_specdata")

files_full <- list.files("specdata", full.names=TRUE)
files_full
tmp <- lapply(files_full, read.csv)
str(tmp)

dat <- do.call(rbind, tmp)
str(dat)

pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length one indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate"
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  dat_subset <- dat[dat$ID %in% id,]            #subsets the rows that match the 'id' argument
  mean(dat_subset[, pollutant], na.rm=TRUE)     #identifies the mean 
                                                #while stripping out the NAs
}