README
================
Alexander Palencia 20160391
9/27/2020

## Lab 5

### Parte 1: Predecir un eclipse solar

``` r
eclipse_init <- dmy_hms('21-08-2017 18:26:40')
synodic <- ddays(29) + dhours(12) + dminutes(44) + dseconds(3)
saros <- 223*synodic
eclip <- eclipse_init + saros
paste("La fecha del siguiente eclipse solar sera: ", eclip)
```

    ## [1] "La fecha del siguiente eclipse solar sera:  2035-09-02 02:09:49"

### Parte 2: Agrupaciones y operaciones con fechas

#### Entendiendo la Data

``` r
data <- read_xlsx("D:/Documents/Semestres UFM/2020/Segundo semestre/Data Wrangling/Data-Wrangling-UFM/lab5/data.xlsx")

summary(data)
```

    ##  Fecha Creación     Hora Creación                   Caller ID     
    ##  Length:263725      Min.   :1899-12-31 00:00:00   Min.   :150432  
    ##  Class :character   1st Qu.:1899-12-31 05:14:20   1st Qu.:447760  
    ##  Mode  :character   Median :1899-12-31 11:28:11   Median :629748  
    ##                     Mean   :1899-12-31 11:31:47   Mean   :631743  
    ##                     3rd Qu.:1899-12-31 17:44:22   3rd Qu.:816614  
    ##                     Max.   :1899-12-31 23:59:59   Max.   :999989  
    ##      Cod                Email             SMS              Call        
    ##  Length:263725      Min.   :0.0000   Min.   :0.0000   Min.   :0.00000  
    ##  Class :character   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.00000  
    ##  Mode  :character   Median :0.0000   Median :1.0000   Median :0.00000  
    ##                     Mean   :0.2361   Mean   :0.7422   Mean   :0.02171  
    ##                     3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:0.00000  
    ##                     Max.   :1.0000   Max.   :1.0000   Max.   :1.00000  
    ##  Fecha Final          Hora Final                 
    ##  Length:263725      Min.   :1899-12-31 00:00:04  
    ##  Class :character   1st Qu.:1899-12-31 05:29:30  
    ##  Mode  :character   Median :1899-12-31 11:42:58  
    ##                     Mean   :1899-12-31 11:46:40  
    ##                     3rd Qu.:1899-12-31 17:59:40  
    ##                     Max.   :1900-01-01 00:20:59

``` r
nrow(data)
```

    ## [1] 263725

``` r
unique(data$Cod)
```

    ## [1] "Cancelaciones"                "Otros/Varios"                
    ## [3] "Consultas"                    "Actualización de Información"
    ## [5] "Cobros"                       "Empresarial"                 
    ## [7] "0"

#### Tenemos 239725 registros, 7 distintos Codigos y notar que la Fecha creacion tiene distintos formatos de fecha.

#### Validando el formato de las columnas, como podemos ver la fecha creación tinen distintos formatos

``` r
# Validate that all values have the same format in column 'hora Creacion'
sum(!is.na(parse_date_time(data$`Hora Creación`,orders="ymd_HMS")))
```

    ## [1] 263725

``` r
# Validate that all values have the same format in column 'hora final'
sum(!is.na(parse_date_time(data$`Hora Final`,orders="ymd_HMS")))
```

    ## [1] 263725

``` r
# Validate that all values have the same format in column 'fecha Creacion'
nrow(parse_date_time(data$`Fecha Creación`,orders="dmy"))
```

    ## Warning: 104237 failed to parse.

    ## NULL

#### Creando dos dataframes con el parseo de las fechas

``` r
# Creating new dataframe parsing date to dmy
df <- data %>%
  mutate(`Fecha Creación` = (dmy(`Fecha Creación`)))
```

    ## Warning: 104237 failed to parse.

``` r
df <- df[!is.na(df$`Fecha Creación`), ]

# Creating new dataframe parsing the string date of execel
df1 <- data
df1$`Fecha Creación` <- as.Date(strtoi(df1$`Fecha Creación`), origin = "1899-12-30")
df1 <- df1[!is.na(df1$`Fecha Creación`), ]

# Merge the two dataframes
dfTotal <- rbind.fill(df1, df)

# Create new column for call duration in seconds
dfTotal$callDuration <- dfTotal$`Hora Final` - dfTotal$`Hora Creación`
```

#### 1\. ¿En qué meses existe una mayor cantidad de llamadas por código?

##### Como podemos ver las consultas se hicieron más en el mes de octubre y las cancelaciones en marzo

``` r
coddsw <- dfTotal %>% 
  group_by(Cod, month(`Fecha Creación`)) %>% 
  tally() %>% 
  top_n(1, n) %>% 
  arrange(desc(n))
head(coddsw)
```

    ## # A tibble: 6 x 3
    ## # Groups:   Cod [6]
    ##   Cod                          `month(\`Fecha Creación\`)`     n
    ##   <chr>                                              <dbl> <int>
    ## 1 Consultas                                             10 10790
    ## 2 Cancelaciones                                          3  4092
    ## 3 Empresarial                                           10  3136
    ## 4 Actualización de Información                           5  1691
    ## 5 0                                                      7  1463
    ## 6 Otros/Varios                                           1  1129

#### 2\. ¿Qué día de la semana es el más ocupado?

##### El día más ocupado es el 1 es decir el Domingo le sigue el jueves

``` r
week_day <- dfTotal %>% 
  group_by(wday(`Fecha Creación`)) %>% 
  tally() %>% 
  arrange(desc(n))
week_day
```

    ## # A tibble: 7 x 2
    ##   `wday(\`Fecha Creación\`)`     n
    ##                        <dbl> <int>
    ## 1                          1 38254
    ## 2                          5 37726
    ## 3                          3 37710
    ## 4                          7 37614
    ## 5                          4 37511
    ## 6                          2 37501
    ## 7                          6 37409

#### 3\. ¿Qué mes es el más ocupado?

##### El més más ocupado es en marzo

``` r
month_busy <- dfTotal %>% 
  group_by(month(`Fecha Creación`)) %>% 
  tally() %>% 
  arrange(desc(n))
month_busy
```

    ## # A tibble: 12 x 2
    ##    `month(\`Fecha Creación\`)`     n
    ##                          <dbl> <int>
    ##  1                           3 22708
    ##  2                          10 22601
    ##  3                           5 22525
    ##  4                           7 22514
    ##  5                           1 22425
    ##  6                           8 22316
    ##  7                          12 22151
    ##  8                           9 21891
    ##  9                          11 21681
    ## 10                           4 21611
    ## 11                           6 21370
    ## 12                           2 19932

#### 4\. ¿Existe una concentración o estacionalidad en la cantidad de llamadas?

##### No existe ni una estacionalidad o concentración en las llamadas ya que podemos ver los llamadas esta distribuidas de una forma casi pareja en los meses.

#### 5\. ¿Cuántos minutos dura la llamada promedio?

``` r
mean_sec <- mean(dfTotal$callDuration)
mean_min <- mean_sec/60
paste('El promedio de las llamadas en segundos es de ', round(mean_sec, 2), ' y en minutos es ', round(mean_min, 2))
```

    ## [1] "El promedio de las llamadas en segundos es de  893.38  y en minutos es  14.89"

#### 6\. Realice una tabla de frecuencias con el tiempo de llamada.

``` r
duration_call <- dfTotal %>% 
  group_by(callDuration) %>% 
  tally() %>% 
  arrange(desc(n))
duration_call
```

    ## # A tibble: 31 x 2
    ##    callDuration     n
    ##    <drtn>       <int>
    ##  1    0 secs     9706
    ##  2   60 secs     8741
    ##  3 1380 secs     8630
    ##  4  780 secs     8614
    ##  5 1200 secs     8578
    ##  6  480 secs     8576
    ##  7 1020 secs     8576
    ##  8  240 secs     8513
    ##  9  120 secs     8508
    ## 10  540 secs     8504
    ## # ... with 21 more rows

### Parte 3: Signo Zodiacal solo acepta como input la fecha en formato dmy

``` r
name_zod <- function(birth) {
  valid_date <- dmy(birth)
  signo <- Zodiac(valid_date)
  return(signo)
}
a <- name_zod('23-10-1997')
paste(' Tu signo es ', a)
```

    ## [1] " Tu signo es  Libra"

No es preciso pero se hizo para demostrar que no es necesario una
libreria como funciona se selecciona el dia que se ingreso y se cuenta
la cantidad de dias que pasaron en base a esto se filtra en el dataset
por un rango y se sabe que signo es

``` r
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
```

### Parte 4: Flights

``` r
# Importando y limpiando la data
tiempo_vuelos <- flights
tiempo_vuelos <- tiempo_vuelos[!is.na(tiempo_vuelos$dep_time), ]
tiempo_vuelos <- tiempo_vuelos[!is.na(tiempo_vuelos$arr_time), ]

# Poniendoles los 0s que falta para poder formatear la hora
tiempo_vuelos$dep_time_valid <- sprintf("%04d", tiempo_vuelos$dep_time)
tiempo_vuelos$arr_time_valid <- sprintf("%04d", tiempo_vuelos$dep_time)
tiempo_vuelos$sched_dep_time_valid <- sprintf("%04d", tiempo_vuelos$sched_dep_time)
tiempo_vuelos$sched_arr_time_valid <- sprintf("%04d", tiempo_vuelos$sched_arr_time)

# Paseando de numero a hora
tiempo_vuelos$valid_dep_time <- format(strptime(tiempo_vuelos$dep_time_valid, format="%H%M"), format = "%H:%M")
tiempo_vuelos$valid_arr_time <- format(strptime(tiempo_vuelos$arr_time_valid, format="%H%M"), format = "%H:%M")
tiempo_vuelos$valid_sched_dep_time <- format(strptime(tiempo_vuelos$sched_dep_time_valid, format="%H%M"), format = "%H:%M")
tiempo_vuelos$valid_sche_arr_time <- format(strptime(tiempo_vuelos$sched_arr_time_valid, format="%H%M"), format = "%H:%M")


# Agregando columna
tiempo_vuelos$Salida <- ymd_hm(paste(tiempo_vuelos$year, tiempo_vuelos$month, tiempo_vuelos$day, tiempo_vuelos$valid_dep_time))

tiempo_vuelos$llegada <- ymd_hm(paste(tiempo_vuelos$year, tiempo_vuelos$month, tiempo_vuelos$day, tiempo_vuelos$valid_arr_time)
                                 )
tiempo_vuelos$Salida_estimada <- ymd_hm(paste(tiempo_vuelos$year, tiempo_vuelos$month, tiempo_vuelos$day, tiempo_vuelos$valid_sched_dep_time))

tiempo_vuelos$llegada_estimada <- ymd_hm(paste(tiempo_vuelos$year, tiempo_vuelos$month, tiempo_vuelos$day, tiempo_vuelos$valid_sche_arr_time))

tiempo_vuelos$Retraso <- tiempo_vuelos$dep_delay + tiempo_vuelos$arr_delay

head(select(tiempo_vuelos, Salida, llegada, Salida_estimada, llegada_estimada, Retraso))
```

    ## # A tibble: 6 x 5
    ##   Salida              llegada             Salida_estimada    
    ##   <dttm>              <dttm>              <dttm>             
    ## 1 2013-01-01 05:17:00 2013-01-01 05:17:00 2013-01-01 05:15:00
    ## 2 2013-01-01 05:33:00 2013-01-01 05:33:00 2013-01-01 05:29:00
    ## 3 2013-01-01 05:42:00 2013-01-01 05:42:00 2013-01-01 05:40:00
    ## 4 2013-01-01 05:44:00 2013-01-01 05:44:00 2013-01-01 05:45:00
    ## 5 2013-01-01 05:54:00 2013-01-01 05:54:00 2013-01-01 06:00:00
    ## 6 2013-01-01 05:54:00 2013-01-01 05:54:00 2013-01-01 05:58:00
    ## # ... with 2 more variables: llegada_estimada <dttm>, Retraso <dbl>
