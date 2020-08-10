library(tidyverse)

files <- list.files(path = "./Data", full.names=TRUE)

read <- function(excel_file)
{
  excel <- read_excel(excel_file, range = cell_cols("A:H"))
  fecha <- substr(excel_file, 8, 14)
  excel$Fecha <- fecha
  return(excel)
}
data <- lapply(files,read)
df <- do.call("rbind",data)

write.csv(df, "MyData.csv",row.names=FALSE)


# Create the function.
calculate_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

v1 <- c(2,1,2,3,1,2,3,4,1,5,5,3,2,3)
v2<- c(5,2,5,9,9,7,5,6,1,3,9,7,4,9)
v3 <- c(4,5,3,6,6,3,3,1,2,5,7,5,8,7)
list_vector <- list(v1,v2,v3)
lapply(list_vector, calculate_mode)


data <- read.delim("./Data/INE_PARQUE_VEHICULAR_080219.txt", header=TRUE, sep="|")
head(data)

