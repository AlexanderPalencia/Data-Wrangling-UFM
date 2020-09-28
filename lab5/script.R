library(nycflights13)
library(dplyr)
library(lubridate)
library(readxl)
library(plyr)
library(DescTools)

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

name_zod <- function(birth) {
  valid_date <- dmy(birth)
  signo <- Zodiac(valid_date)
  return(signo)
}
a <- name_zod('23-10-1997')
paste(' Tu signo es ', a)

# No es preciso pero se hizo para demostrar que no es necesario una libreria
# como funciona se selecciona el dia que se ingreso y se cuenta la cantidad de dias que pasaron
# en base a esto se filtra en el dataset por un rango y se sabe que signo es
zod_name <- c('Capricornio','Acuario', 'Piscis', 'Aries', 'Tauro', 'Geminis', 'Cancer', 'Leo', 'Virgo', 'Libra', 'Escorpio', 'Sagitario', 'Capricornio')
zod_day <- c('21', '50', '80', '110', '140', '170', '200', '230', '260', '290', '310', '340', '365')
zod_day_fin <- c('0', '21', '51', '110', '140', '170', '200', '230', '260', '290', '310', '340', '365')
zod_df <- data.frame(zod_name, zod_day)
zod <- function(input) {
  valid_date <- dmy(input)
  init_year <- year(valid_date)
  init_date <- ymd(paste(init_year, 1, 1))
  days_diff <- valid_date - init_date
  paste('fecha inicio ', days_diff)
  #select range
}



# Parte 4: Flights
tiempo_vuelos <- flights
tiempo_vuelos <- tiempo_vuelos[!is.na(tiempo_vuelos$dep_time), ]
tiempo_vuelos <- tiempo_vuelos[!is.na(tiempo_vuelos$arr_time), ]

tiempo_vuelos$dep_time_valid <- sprintf("%04d", tiempo_vuelos$dep_time)
tiempo_vuelos$arr_time_valid <- sprintf("%04d", tiempo_vuelos$dep_time)
tiempo_vuelos$sched_dep_time_valid <- sprintf("%04d", tiempo_vuelos$sched_dep_time)
tiempo_vuelos$sched_arr_time_valid <- sprintf("%04d", tiempo_vuelos$sched_arr_time)

tiempo_vuelos$valid_dep_time <- format(strptime(tiempo_vuelos$dep_time_valid, format="%H%M"), format = "%H:%M")
tiempo_vuelos$valid_arr_time <- format(strptime(tiempo_vuelos$arr_time_valid, format="%H%M"), format = "%H:%M")
tiempo_vuelos$valid_sched_dep_time <- format(strptime(tiempo_vuelos$sched_dep_time_valid, format="%H%M"), format = "%H:%M")
tiempo_vuelos$valid_sche_arr_time <- format(strptime(tiempo_vuelos$sched_arr_time_valid, format="%H%M"), format = "%H:%M")

tiempo_vuelos$Salida <- ymd_hm(paste(tiempo_vuelos$year, tiempo_vuelos$month, tiempo_vuelos$day, tiempo_vuelos$valid_dep_time))
tiempo_vuelosLlegada <- ymd_hm(paste(tiempo_vuelos$year, tiempo_vuelos$month, tiempo_vuelos$day, tiempo_vuelos$valid_arr_time))
tiempo_vuelos$Salida_estimada <- ymd_hm(paste(tiempo_vuelos$year, tiempo_vuelos$month, tiempo_vuelos$day, tiempo_vuelos$valid_sched_dep_time))
tiempo_vuelos$llegada_estimada <- ymd_hm(paste(tiempo_vuelos$year, tiempo_vuelos$month, tiempo_vuelos$day, tiempo_vuelos$valid_sche_arr_time))
tiempo_vuelos$Retraso <- tiempo_vuelos$dep_delay + tiempo_vuelos$arr_delay
