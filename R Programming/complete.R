setwd("C:/Users/stephen.l.kingery/Desktop/Coursera/rprog_data_specdata")

files_full <- list.files("specdata", full.names=TRUE)
tmp <- lapply(files_full, read.csv)
dat <- do.call(rbind, tmp)

complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases

  dat$nobs <- 0
  df <- dat[FALSE,]
  
  for (i in id) {
    dat_subset <- dat[dat$ID == i, ]
    dat_complete <- dat_subset[complete.cases(dat_subset),]
    if (nrow(dat_complete) > 0) {
      dat_complete$nobs <- nrow(dat_complete)
      df <- rbind(df, dat_complete)
    } else {
      df <- rbind(df, dat_subset)
    }
  }
  
  complete_out <<- unique(df[, c("ID","nobs")])
  rownames(complete_out) <- NULL
  colnames(complete_out) <- c("id","nobs")
  complete_out
}