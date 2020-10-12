library(dplyr)
library(lubridate)
library(tidyverse)
library(reshape2)
library(rapportools)
library("writexl")
library(ggplot2)

# "D:/Documents/Semestres UFM/2020/Segundo semestre/Data Wrangling/Data-Wrangling-UFM/lab7"
data <- read.csv(file = "./c1.csv", header = TRUE)
## Erase null columns
data$X.1 <- NULL
data$X.2 <- NULL
data$X.3 <- NULL
data$X.4 <- NULL
data$X.5 <- NULL
data$X <- NULL

# Data cleaning numeric columns 
numeric_col <- c("Camion_5", "Pickup", "Moto", "factura", "directoCamion_5", "directoPickup", "directoMoto", "fijoCamion_5", "fijoPickup", "fijoMoto")
# dealing with null values
data[numeric_col] <- lapply(data[numeric_col], function(x) ifelse(x == " Q-   ", NA, x))
# elimination of the Q symbol
data[numeric_col] <- lapply(data[numeric_col], function(x) gsub("Q", '', x))
# transforming a number
data[numeric_col] <- lapply(data[numeric_col], function(x) as.numeric(x))

# Data cleaning time columns 
time_col <- c('X5.30', 'X30.45', 'X45.75', 'X75.120', 'X120.')
# dealing with null values
data[time_col] <- lapply(data[time_col], function(x) ifelse(is.empty(x), NA, TRUE))


# Converting data to tidy mode time (duration variable)
time_tidyData <- melt(data = data, id.vars = c("Fecha", "ID", "Camion_5", "Pickup","Moto","Cod","origen", "Lat","Long", "factura","directoCamion_5","directoPickup", "directoMoto", "fijoCamion_5", "fijoPickup", "fijoMoto", "height")) %>% drop_na(value) 
names(time_tidyData)[names(time_tidyData) == 'variable'] <- 'time_duration'
time_tidyData$value <- NULL

# Converting data to tidy mode costs (fixed costs)
fijo_tidyData <- melt(data = time_tidyData, id.vars = c("Fecha", "ID", "Camion_5", "Pickup","Moto","Cod","origen", "Lat","Long", "factura","directoCamion_5","directoPickup", "directoMoto", "height", "time_duration")) %>% drop_na(value) 
names(fijo_tidyData)[names(fijo_tidyData) == 'variable'] <- 'transporte_costo_fijo'
names(fijo_tidyData)[names(fijo_tidyData) == 'value'] <- 'costo_fijo_cantidad'

# Converting data to tidy mode costs (direct costs)
directo_tidyData <- melt(data = fijo_tidyData, id.vars = c("Fecha", "ID", "Camion_5", "Pickup","Moto","Cod","origen", "Lat","Long", "factura", "height", "time_duration", "transporte_costo_fijo", "costo_fijo_cantidad")) %>% drop_na(value) 
names(directo_tidyData)[names(directo_tidyData) == 'variable'] <- 'transporte_costo_directo'
names(directo_tidyData)[names(directo_tidyData) == 'value'] <- 'costo_directo_cantidad'

# Converting data to tidy mode cost total Transport (direct costs + fixed cost)
tidyData <- melt(data = directo_tidyData, id.vars = c("Fecha", "ID", "Cod","origen", "Lat","Long", "factura", "height", "time_duration", "transporte_costo_fijo", "costo_fijo_cantidad", "transporte_costo_directo", "costo_directo_cantidad")) %>% drop_na(value) 
names(tidyData)[names(tidyData) == 'variable'] <- 'transporte_utilizado'
names(tidyData)[names(tidyData) == 'value'] <- 'costo_total'

# Round to 2 decimals numeric columns
data[numeric_col] <- lapply(data[numeric_col], function(x) format(round(x, 2), nsmall = 2))

# Adding costos columns (direct costs + fixed cost)
tidyData$costos <- tidyData$costo_directo_cantidad + tidyData$costo_fijo_cantidad

# validation of total cost is correct
tidyData$resta <- tidyData$costo_total - tidyData$costos
tidyData$resta <- format(round(tidyData$resta, 2), nsmall = 2)
tidyData$resta <- lapply(tidyData$resta, function(x) as.numeric(x))
tidyData$validacion_costos <- ifelse(tidyData$resta == 0.00, TRUE, FALSE)

# Converting to valid date format
tidyData$Fecha <- parse_date_time(tidyData$Fecha,orders="dmy")

# adding profit column
tidyData$ganancias <- tidyData$factura - tidyData$costo_total


## Analisis de la empresa

# No existen servicios que reporten perdidas
nrow(filter(tidyData, ganancias < 0))

# Existen 10 distintos servicios que se dieron en el a???o 2017
servicios <- unique(tidyData$Cod)
servicios
length(servicios)

# Existen 4 distintas sedes en el a???o 2017
sedes <- unique(tidyData$origen)
sedes
length(sedes)

# Se le dieron mantenimiento a 74239 postes distintos en el a???o 2017
n_postes <- length(unique(tidyData$ID))

# Nuestros precios oscilan en un rango de 7.3 - 521.67 el mas caro fue un CAMBIO_FUSIBLE con camion mientras el menor
filter(tidyData, factura == max(tidyData$factura))
filter(tidyData, factura == min(tidyData$factura))

# Nuestros costos oscilan en un rango de 0.96 - 467.93 el mayor es de REVISION_TRANSFORMADOR con camion y el menor es de CAMBIO_FUSIBLE
filter(tidyData, costo_total == max(tidyData$costo_total))
filter(tidyData, costo_total == min(tidyData$costo_total))

# Nuestras ganancias oscilan de 5 - 100, no existen ganancias mayores a 100 que interesante ni menores a 5
max(tidyData$ganancias)
min(tidyData$ganancias)


# Existe una inconsitencia de 0.25 centavos en los costos es decir que la suma de los costos no da la suma de los costos totales esta variaciones son de un centevo
a <- sum(tidyData$costo_total)
sprintf("%f", a)

b <- sum(tidyData$costos)
sprintf("%f", b)


# Estado de resultados general 2017
costos_totales <- sum(tidyData$costo_total)
ingresos_totales <- sum(tidyData$factura)
utilidad_bruta <- ingresos_totales - costos_totales
sprintf("Costos totales %f", costos_totales)
sprintf("Ingresos totales %f", ingresos_totales)
sprintf("Utilidad bruta %f", utilidad_bruta)


round_col <- c('costo', 'ingreso', 'ganancias', 'rentabilidad', 'promedio_costos', 'promedio_ingresos', 'promedio_ganacia')



# El mes que mas ganancia tuvo fue Julio con 733,610 pero el que m???s servicios tiene es octubre

# agrupacion por mes
monts_data <- tidyData %>% 
  group_by(month(Fecha))

# tabla por mes
df_month <- monts_data  %>% summarise(
  cantidad_servicios = length(ID),
  costo = sum(costo_total),
  ingreso = sum(factura),
  ganancias = (sum(factura) - sum(costo_total)),
  rentabilidad = ((ganancias/ingreso)*100),
  promedio_costos = mean(costo_total),
  promedio_ingresos = mean(factura),
  promedio_ganacia = (promedio_ingresos - promedio_costos)) %>%
  arrange(desc(ganancias))

df_month[round_col] <- lapply(df_month[round_col], function(x) format(round(x, 2), nsmall = 2))

write_xlsx(df_month, './df_month.xlsx')

# El poste con m???s ingresos  rentable es el 863979 con ganancias de 14799
# Agrupacion por poste
id_data <- tidyData %>% 
  group_by(ID)

# Cantidad de servicio a poste
df_poste <- id_data  %>% summarise(
  cantidad_servicios = length(ID),
  costo = sum(costo_total),
  ingreso = sum(factura),
  ganancias = (sum(factura) - sum(costo_total)),
  rentabilidad = ((ganancias/ingreso)*100),
  promedio_costos = mean(costo_total),
  promedio_ingresos = mean(factura),
  promedio_ganacia = (promedio_ingresos - promedio_costos)) %>%
  arrange(desc(ganancias))

df_poste[round_col] <- lapply(df_poste[round_col], function(x) format(round(x, 2), nsmall = 2))

write_xlsx(df_poste, './df_poste.xlsx')

# Postes mas rentables
rentabilidad_postes <- df_poste %>% arrange(desc(rentabilidad))
write_xlsx(rentabilidad_postes, './rentabilidad_df_poste.xlsx')


# Ley de Pareto el 20% de mis clientes (postes) representa el 54.39% de mis ganancias
ganancia_20p_cliente <- sum(df_poste[1:14848,]$ganancias)
porcentaje_ganancia <- (ganancia_20p_cliente * 100)/utilidad_bruta
sprintf("Porcentaje %f", porcentaje_ganancia)





# Existen dos distribuidoras que tienen mucha demanda y cuentan con los mayores ingresos pero podemos ver que la rentabilidad de todas las sedes son muy parecidas.
# Agrupacion por origen
origen_data <- tidyData %>% 
  group_by(origen)

# Cantidad de servicio por origen
df_origen <- origen_data  %>% summarise(
  cantidad_servicios = length(ID),
  costo = sum(costo_total),
  ingreso = sum(factura),
  ganancias = (sum(factura) - sum(costo_total)),
  rentabilidad = ((ganancias/ingreso)*100),
  promedio_costos = mean(costo_total),
  promedio_ingresos = mean(factura),
  promedio_ganacia = (promedio_ingresos - promedio_costos)) %>%
  arrange(desc(ganancias))


df_origen[round_col] <- lapply(df_origen[round_col], function(x) format(round(x, 2), nsmall = 2))

write_xlsx(df_poste, './df_origen.xlsx')





# El servicio mas ganacia tuvo es el de revision
# Agrupacion por Servicio
cod_data <- tidyData %>% 
  group_by(Cod)

# Cantidad de servicio por cod
df_cod <- cod_data  %>% summarise(
  cantidad_servicios = length(ID),
  costo = sum(costo_total),
  ingreso = sum(factura),
  ganancias = (sum(factura) - sum(costo_total)),
  rentabilidad = ((ganancias/ingreso)*100),
  promedio_costos = mean(costo_total),
  promedio_ingresos = mean(factura),
  promedio_ganacia = (promedio_ingresos - promedio_costos)) %>%
  arrange(desc(ganancias))


df_cod[round_col] <- lapply(df_cod[round_col], function(x) format(round(x, 2), nsmall = 2))

write_xlsx(df_cod, './df_cod.xlsx')

# Rentabilidad de servicios
rentabilidad_cod <- df_cod %>% arrange(desc(rentabilidad))
write_xlsx(rentabilidad_cod, './rentabilidad_sevicios.xlsx')


rentabilidad_cod['rentabilidad'] <- lapply(rentabilidad_cod['rentabilidad'], function(x) as.numeric(x))

#Grafica rentabilidad por transporte
barplot(rentabilidad_cod$rentabilidad, names.arg = rentabilidad_cod$Cod   , main="Rentabilidad por servicio", cex.names = 0.7)




# Agrupacion por Transporte utilizado
transporte_data <- tidyData %>% 
  group_by(transporte_utilizado)

# Cantidad de Transportes
df_transporte <- transporte_data  %>% summarise(
  cantidad_servicios = length(ID),
  costo = sum(costo_total),
  ingreso = sum(factura),
  ganancias = (sum(factura) - sum(costo_total)),
  rentabilidad = ((ganancias/ingreso)*100),
  promedio_costos = mean(costo_total),
  promedio_ingresos = mean(factura),
  promedio_ganacia = (promedio_ingresos - promedio_costos)) %>%
  arrange(desc(ganancias))

df_transporte[round_col] <- lapply(df_transporte[round_col], function(x) format(round(x, 2), nsmall = 2))

write_xlsx(df_transporte, './df_transporte.xlsx')

# Rentabilidad de transporte
rentabilidad_transporte <- df_transporte %>% arrange(desc(rentabilidad))
write_xlsx(rentabilidad_transporte, './rentabilidad_transporte.xlsx')


rentabilidad_transporte['rentabilidad'] <- lapply(rentabilidad_transporte['rentabilidad'], function(x) as.numeric(x))
#Grafica rentabilidad por transporte
barplot(rentabilidad_transporte$rentabilidad, names.arg = rentabilidad_transporte$transporte_utilizado  , main="Rentabilidad por transporte", cex.names = 0.7)







# Algo interesante es que las motos solo se utilizan para un servicio por ejemplo hacer todas las visitas en moto y no en camion o pickup
# Agrupacion por Transporte y servicio
transporte_servicio <- tidyData %>% 
  group_by(transporte_utilizado, Cod)

# Cantidad de transporte servicio
df_transporte_servicio <- transporte_servicio  %>% summarise(
  cantidad_servicios = length(ID),
  costo = sum(costo_total),
  ingreso = sum(factura),
  ganancias = (sum(factura) - sum(costo_total)),
  rentabilidad = ((ganancias/ingreso)*100),
  promedio_costos = mean(costo_total),
  promedio_ingresos = mean(factura),
  promedio_ganacia = (promedio_ingresos - promedio_costos)) %>%
  arrange(desc(ganancias))

df_transporte_servicio[round_col] <- lapply(df_transporte_servicio[round_col], function(x) format(round(x, 2), nsmall = 2))

write_xlsx(df_transporte_servicio, './df_transporte_servicio.xlsx')

# Rentabilidad de transporte y servico
rentabilidad_transporte_servicio <- df_transporte_servicio %>% arrange(desc(rentabilidad))
write_xlsx(rentabilidad_transporte_servicio, './rentabilidad_transporte_servicio.xlsx')


rentabilidad_transporte_servicio['rentabilidad'] <- lapply(rentabilidad_transporte_servicio['rentabilidad'], function(x) as.numeric(x))
#Grafica rentabilidad por transporte
barplot(rentabilidad_transporte_servicio$rentabilidad, names.arg = rentabilidad_transporte_servicio$transporte_utilizado  , main="Rentabilidad por transporte", cex.names = 0.7)





# Los tiempos de servicios es de 75 a 120 min y representan nuestro ingresos y ganancias sin embargo el tiempo mas rentable es el de 5 a 30 min.
# Agrupacion por tiempo
aaaa <- tidyData %>% 
  group_by(time_duration)

# Cantidad de servicio por tiempo
df_tiempo <- aaaa  %>% summarise(
  cantidad_servicios = length(ID),
  costo = sum(costo_total),
  ingreso = sum(factura),
  ganancias = (sum(factura) - sum(costo_total)),
  rentabilidad = ((ganancias/ingreso)*100),
  promedio_costos = mean(costo_total),
  promedio_ingresos = mean(factura),
  promedio_ganacia = (promedio_ingresos - promedio_costos)) %>%
  arrange(desc(ganancias))

df_tiempo[round_col] <- lapply(df_tiempo[round_col], function(x) format(round(x, 2), nsmall = 2))

write_xlsx(df_tiempo, './df_tiempo.xlsx')


#Grafica rentabilidad por tiempo
barplot(df_tiempo$rentabilidad, names.arg = df_tiempo$time_duration  , main="Efectividad de Reparaciones por servicio", cex.names = 0.7)




# Agrupacion por tiempo y servicio
sss <- tidyData %>% 
  group_by(time_duration, Cod)

# Cantidad de servicio por cod y tiempo
df_tiempo_servicio <- sss  %>% summarise(
  cantidad_servicios = length(ID),
  costo = sum(costo_total),
  ingreso = sum(factura),
  ganancias = (sum(factura) - sum(costo_total)),
  rentabilidad = ((ganancias/ingreso)*100),
  promedio_costos = mean(costo_total),
  promedio_ingresos = mean(factura),
  promedio_ganacia = (promedio_ingresos - promedio_costos)) %>%
  arrange(desc(ganancias))

df_tiempo_servicio[round_col] <- lapply(df_tiempo_servicio[round_col], function(x) format(round(x, 2), nsmall = 2))

write_xlsx(df_tiempo_servicio, './df_tiempo_servicio.xlsx')

# Rentabilidad de tiempo y servicio
rentabilidad_tiempo_servicio <- df_tiempo_servicio %>% arrange(desc(rentabilidad))
write_xlsx(rentabilidad_tiempo_servicio, './rentabilidad_tiempo_servicio.xlsx')


# se tuvo una perdida de 2128519 respecto al 2017 es decir una utilidad total de 6385558 para el a???o 2019 se espera aumentar en un 10% es decir lograr tener una utilidad de 7024114

utilidad_2018 <- utilidad_bruta - utilidad_bruta*0.25
perdidad_2018 <- utilidad_bruta - utilidad_2018
utilidad_2019 <- utilidad_2018 * 1.10




