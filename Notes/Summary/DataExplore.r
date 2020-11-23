library(readr)
library(dplyr)
library(plotly)
library(DataExplorer)


df <- read_delim()







# Structure and summary -------------------------------------------------
str(df)
glimpse(df)
plot_str(df)
introduce(df) %>% View()

summary(df)

# Convert days to years (age)
df <- df %>% 
  mutate(age = floor(age/365.25))

# missing data --------------------------------------------------------------
colSums(is.na(df))
plot_missing(df)
summary(df)

# Histogramas -------------------
hist()

hist_plt <- function(df, var, norm = "normal", bins = 30){
  # da error seleccionar normar
  df_columna <- df %>% select(var) 
  
  
  # Se cambia la variable a un objeto
  var <- sym(var)
  
}






