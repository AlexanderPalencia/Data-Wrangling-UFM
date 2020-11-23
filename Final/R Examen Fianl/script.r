library(dplyr)
library(lubridate)
library(tidyverse)
library(reshape2)
library(rapportools)
library("writexl")
library(ggplot2)
library(readr)
library(stringr)
library(formattable)
library(devtools)
library(plotly)
library(matrixStats)
require(tidyr)
require(DataExplorer)
require(corrplot)



data <- read.csv(file = "./c1.csv", header = TRUE)
f <- read_delim("2010-2019-top.csv",
                ";", escape_double = FALSE, trim_ws = TRUE)
excel<-read_excel('example_1.xlsx')


#df$title <- iconv(df$title, to = "UTF-8")
rename(df,top_genre = `top genre`)
rename_with(df, ~(gsub(" ","_",.x)))

## Select data
select(df, artist, year)
df %>%
  select(artist,title,year) %>%
  filter(year == 2010)

# convert columns
df<-mutate_if(df,is.character, as.factor)



# Group by
df %>%
  select(year,artist) %>%
  group_by(year) %>%
  summarise(n = n())

df %>%
  summarise(n = n_distinct(artist))

df %>%
  group_by(title) %>%
  summarise(freq = n()) %>%
  #ungroup() %>%
  group_by(freq) %>%
  summarise(n()) %>%
  arrange(desc(artist_songs))

#str_sub() nos permite extraer caracteres de un vector

# Covert to tidy
# Melt, Gather, pivot_longer
# time lubridate



#Check for missing data
colSums(is.na(df))
plot_missing(df)











