library(dplyr)
library(readr)

column_classes <- c("character", "character", "double", "double", "double", "double", "double") 
mananera <- list.files(path="./Datos/", full.names = TRUE) %>% 
  lapply(read_csv, colClasses= column_classes) %>% 
  bind_rows 

  

file.paths <- list.files(pattern = '.csv')

column_classes <- c("character", "character", "double", "double", "double", "double", "double") # specify for all columns 
locale <- "ISO-8859-1"

my.files <- lapply(file.paths, function(x) read.csv(x, colClasses= column_classes, fileEncoding="UTF-8")) %>% 
  bind_rows 

write.csv (my.files, "mananeras.csv")
