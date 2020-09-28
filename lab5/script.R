library(nycflights13)
library(dplyr)
library(lubridate)
library(readxl)
library(plyr)

eclipse_init <- dmy_hms('21-08-2017 18:26:40')
synodic <- ddays(29) + dhours(12) + dminutes(44) + dseconds(3)
saros <- 223*synodic
eclip <- eclipse_init + saros
paste("La fecha del siguiente eclipse solar sera: ", eclip)


data <- read_xlsx("D:/Documents/Semestres UFM/2020/Segundo semestre/Data Wrangling/Data-Wrangling-UFM/lab5/data.xlsx")

summary(data)
nrow(data)
length(is.na(data$`Fecha Creación`))
unique(data$Cod)
length(data$`Caller ID`)

# Validate that all values have the same format in column 'hora Creacion'
sum(!is.na(parse_date_time(data$`Hora Creación`,orders="ymd_HMS")))
# Validate that all values have the same format in column 'hora final'
sum(!is.na(parse_date_time(data$`Hora Final`,orders="ymd_HMS")))
# Validate that all values have the same format in column 'fecha Creacion'
sum((parse_date_time(data$`Fecha Creación`,orders="dmy")))

# Creating new dataframe parsing date to dmy
df <- data %>%
  mutate(`Fecha Creación` = (dmy(`Fecha Creación`)))
df <- df[!is.na(df$`Fecha Creación`), ]

# Creating new dataframe parsing the string date of execel
df1 <- data
df1$`Fecha Creación` <- as.Date(strtoi(df1$`Fecha Creación`), origin = "1899-12-30")
df1 <- df1[!is.na(df1$`Fecha Creación`), ]

# Merge the two dataframes
dfTotal <- rbind.fill(df1, df)

# Create new column for call duration in seconds
dfTotal$callDuration <- dfTotal$`Hora Final` - dfTotal$`Hora Creación`

# 1.	¿En qué meses existe una mayor cantidad de llamadas por código?
cod1 <- dfTotal %>% 
  group_by(Cod, month(`Fecha Creación`)) %>% 
  tally() %>% 
  top_n(1, n) %>% 
  arrange(desc(n))


# 2.	¿Qué día de la semana es el más ocupado?
week_day <- dfTotal %>% 
  group_by(wday(`Fecha Creación`)) %>% 
  tally() %>% 
  arrange(desc(n))

# 3.	¿Qué mes es el más ocupado?
month_busy <- dfTotal %>% 
  group_by(month(`Fecha Creación`)) %>% 
  tally() %>% 
  arrange(desc(n))

# 4.	¿Existe una concentración o estacionalidad en la cantidad de llamadas?
# No existe ni una estacionalidad o concentración en las llamadas ya que podemos ver los llamadas esta distribuidas de una forma casi pareja en los meses.

# 5.	¿Cuántos minutos dura la llamada promedio?
mean_sec <- mean(dfTotal$callDuration)
mean_min <- mean_sec/60
paste('El promedio de las llamadas en segundos es de ', round(mean_sec, 2), ' y en minutos es ', round(mean_min, 2))

# 6.	Realice una tabla de frecuencias con el tiempo de llamada.
duration_call <- dfTotal %>% 
  group_by(callDuration) %>% 
  tally() %>% 
  arrange(desc(n))


# Parte 3: Signo Zodiacal solo acepta como input la fecha en formato dmy
zod_name <- c('Aries', 'Tauro', 'Geminis', 'Cancer', 'Leo', 'Virgo', 'Libra', 'Escorpio', 'Sagitario', 'Capricornio', 'Acuario', 'Piscis')
zod_day <- c('21', '21', '21', '22', '23', '23', '23', '23', '23', '22', '21', '19')
zod_month <- c('3','4', '5', '6', '7', '8', '9', '10', '11', '12', '1', '2')
zdf_df <- as.data.frame(zod_name, zod_day, zod_month)
zod <- function(input) {
  valid_date <- dmy(input)
  
  
}


# Parte 4: Flights


