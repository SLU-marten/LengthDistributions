## code to prepare `data` dataset goes here
data <- read.table("Langder_master.csv", sep = ";", header = T, fileEncoding="latin1")
usethis::use_data_raw(data, overwrite = TRUE)
