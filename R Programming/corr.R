setwd("C:/Users/stephen.l.kingery/Desktop/Coursera/rprog_data_specdata")

files_full <- list.files("specdata", full.names = TRUE)
tmp <- lapply(files_full, read.csv)
dat <- do.call(rbind, tmp)

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a number vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  complete("specdata")
  thresh <<- complete_out[complete_out$nobs >= threshold,]
  thresh_full <<- merge(dat[complete.cases(dat),], thresh, by = "ID")
  as.vector(by(thresh_full[, 3:4], thresh_full$ID, function(thresh_full) {
    cor(thresh_full$sulfate, thresh_full$nitrate)
  }), mode = "numeric")
}
