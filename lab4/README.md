Lab 4
================
Alexander Palencia 20160391
8/27/2020

``` r
require(dplyr)
```

    ## Loading required package: dplyr

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
data <- read.csv(file = "tabla_completa.csv", header = TRUE, encoding = "latin1")
```

``` r
print('Resumen de la data')
```

    ## [1] "Resumen de la data"

``` r
glimpse(data)
```

    ## Rows: 2,180
    ## Columns: 11
    ## $ X         <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17...
    ## $ COD_VIAJE <int> 10000001, 10000002, 10000003, 10000004, 10000005, 1000000...
    ## $ CLIENTE   <chr> "EL PINCHE OBELISCO / Despacho a cliente", "TAQUERIA EL C...
    ## $ UBICACION <int> 76002, 76002, 76002, 76002, 76001, 76001, 76002, 76001, 7...
    ## $ CANTIDAD  <int> 1200, 1433, 1857, 339, 1644, 1827, 1947, 1716, 1601, 1343...
    ## $ PILOTO    <chr> "Fernando Mariano Berrio", "Hector Aragones Frutos", "Ped...
    ## $ Q         <dbl> 300.00, 358.25, 464.25, 84.75, 411.00, 456.75, 486.75, 42...
    ## $ CREDITO   <int> 30, 90, 60, 30, 30, 30, 90, 60, 30, 90, 60, 30, 60, 90, 9...
    ## $ UNIDAD    <chr> "Camion Grande", "Camion Grande", "Camion Grande", "Panel...
    ## $ MES       <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    ## $ ANIO      <int> 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 201...

``` r
print('Count distinct')
```

    ## [1] "Count distinct"

``` r
data %>% summarise_all(n_distinct)
```

    ##      X COD_VIAJE CLIENTE UBICACION CANTIDAD PILOTO    Q CREDITO UNIDAD MES ANIO
    ## 1 2180      2180      18         2     1279      9 1279       3      3  11    1

``` r
ingresoTotal <- sum(data$Q)
sprintf("Ingreso total del año %f", ingresoTotal)
```

    ## [1] "Ingreso total del año 598848.250000"

``` r
print('Clientes de Distribuidora del Sur, S.A.')
```

    ## [1] "Clientes de Distribuidora del Sur, S.A."

``` r
unique(data[["CLIENTE"]])
```

    ##  [1] "EL PINCHE OBELISCO / Despacho a cliente"           
    ##  [2] "TAQUERIA EL CHINITO |||Faltante"                   
    ##  [3] "TIENDA LA BENDICION / Despacho a cliente"          
    ##  [4] "TAQUERIA EL CHINITO"                               
    ##  [5] "CHICHARRONERIA EL RICO COLESTEROL |||Faltante"     
    ##  [6] "UBIQUO LABS |||FALTANTE"                           
    ##  [7] "EL GALLO NEGRO |||DEVOLUCION"                      
    ##  [8] "EL GALLO NEGRO / Despacho a cliente"               
    ##  [9] "UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente"
    ## [10] "POLLO PINULITO|||FALTANTE"                         
    ## [11] "SPORTA, S.A./Despacho a cliente |||Faltante"       
    ## [12] "HOSPITAL ROOSEVELT / Despacho a cliente"           
    ## [13] "UBIQUO LABS"                                       
    ## [14] "BAR LA OFICINA"                                    
    ## [15] "ABARROTERIA EBENEZER/Despacho a cliente"           
    ## [16] "EL PINCHE OBELISCO |||Faltante"                    
    ## [17] "POLLO PINULITO/Despacho a cliente"                 
    ## [18] "HOSPITAL LAS AMERICAS"

``` r
print('Tipo de unidades Distribuidora del Sur, S.A.')
```

    ## [1] "Tipo de unidades Distribuidora del Sur, S.A."

``` r
unique(data[["UNIDAD"]])
```

    ## [1] "Camion Grande"  "Panel"          "Camion Pequeño"

``` r
print(unique(data[["UBICACION"]]))
```

    ## [1] 76002 76001

``` r
ubicacionA <- filter(data, UBICACION == 76002)
ubicacionB <- filter(data, UBICACION == 76001)
```

``` r
print('Pilotos de Distribuidora del Sur, S.A.')
```

    ## [1] "Pilotos de Distribuidora del Sur, S.A."

``` r
unique(data[["PILOTO"]])
```

    ## [1] "Fernando Mariano Berrio"       "Hector Aragones Frutos"       
    ## [3] "Pedro Alvarez Parejo"          "Angel Valdez Alegria"         
    ## [5] "Juan Francisco Portillo Gomez" "Luis Jaime Urbano"            
    ## [7] "Ismael Rodero Monteagudo"      "Felipe Villatoro"             
    ## [9] "Hector Giron"

``` r
print('Clientes faltas')
```

    ## [1] "Clientes faltas"

``` r
Clientes <- as.character(unique(data[["CLIENTE"]]))
Faltantes <- Clientes[c(2,5,6,10,11,16)]
Faltantes
```

    ## [1] "TAQUERIA EL CHINITO |||Faltante"              
    ## [2] "CHICHARRONERIA EL RICO COLESTEROL |||Faltante"
    ## [3] "UBIQUO LABS |||FALTANTE"                      
    ## [4] "POLLO PINULITO|||FALTANTE"                    
    ## [5] "SPORTA, S.A./Despacho a cliente |||Faltante"  
    ## [6] "EL PINCHE OBELISCO |||Faltante"

``` r
Devolucion <- filter(data, CLIENTE == Clientes[7])
sprintf("Cantidad de devoluciones %i", nrow(Devolucion))
```

    ## [1] "Cantidad de devoluciones 119"

``` r
sprintf("Cantidad de devoluciones %f", sum(Devolucion$Q))
```

    ## [1] "Cantidad de devoluciones 33070.500000"

``` r
print('Cantidad Maxima y minima de un camion Pequeño')
```

    ## [1] "Cantidad Maxima y minima de un camion Pequeño"

``` r
s <- filter(data, UNIDAD == "Camion Pequeño")
max(s$CANTIDAD)
```

    ## [1] 1000

``` r
min(s$CANTIDAD)
```

    ## [1] 502

``` r
print('Cantidad Maxima y minima de un camion Grande')
```

    ## [1] "Cantidad Maxima y minima de un camion Grande"

``` r
g <- filter(data, UNIDAD == "Camion Grande")
max(g$CANTIDAD)
```

    ## [1] 1996

``` r
min(g$CANTIDAD)
```

    ## [1] 1002

``` r
print('Cantidad Maxima y minima de un panel')
```

    ## [1] "Cantidad Maxima y minima de un panel"

``` r
g <- filter(data, UNIDAD == "Panel")
max(g$CANTIDAD)
```

    ## [1] 500

``` r
min(g$CANTIDAD)
```

    ## [1] 200

## ¿Debemos invertir en la contratación de más personal?

  - No se deberia contratar mas personal, porque la carga laboral
    reflejada es de un viaje al dia, la siguiente función crea un
    dataframe con la cantidad de viajes por mes, seguido de mostrarnos
    los datos de Viajes Totales, Viajes promedio por mes, Viajes
    repartidos por los 9 pilotos y viajes diarios.

<!-- end list -->

``` r
CreateDF <- function(){
  dataFrame <- data.frame(
    Mes = integer(),
    Cantidad_Viajes = integer(),
    stringsAsFactors = FALSE
  )
  for(i in 1:11){
    ViajesMes <- filter(data, MES == i)
    vec <- c(i,nrow(ViajesMes))
    dataFrame <- rbind(dataFrame, vec)
  }
  names(dataFrame)[1] <- "Mes"
  names(dataFrame)[2] <- "Cantidad_Viajes"
  return(dataFrame)
}
df <- CreateDF()
TotalViajes <- sum(df$Cantidad_Viajes)
sprintf("Viajes totales en el año %i", TotalViajes)
```

    ## [1] "Viajes totales en el año 2180"

``` r
PromedioViajesMes <- mean(df$Cantidad_Viajes)
sprintf("Viajes Promedio en el mes %f", PromedioViajesMes)
```

    ## [1] "Viajes Promedio en el mes 198.181818"

``` r
ViajesPorPilotoMes <- df$Cantidad_Viajes/9
"Viajes Promedio por piloto en los meses"
```

    ## [1] "Viajes Promedio por piloto en los meses"

``` r
ViajesPorPilotoMes
```

    ##  [1] 21.33333 22.55556 20.22222 21.77778 23.88889 21.88889 23.44444 22.11111
    ##  [9] 20.88889 22.22222 21.88889

``` r
ViajesDiarioPilot <- ViajesPorPilotoMes/22
sprintf("Viajes Promedio por piloto diarios ")
```

    ## [1] "Viajes Promedio por piloto diarios "

``` r
ViajesDiarioPilot
```

    ##  [1] 0.9696970 1.0252525 0.9191919 0.9898990 1.0858586 0.9949495 1.0656566
    ##  [8] 1.0050505 0.9494949 1.0101010 0.9949495

## ¿Debemos invertir en la compra de más vehículos de distribución? ¿Cuántos y de que tipo?

  - Se deberia inveertir en camiones grandes porque como podemos ver son
    los mas rentables y los que aportan mas beneficio a la empresa,
    ademas podemos notar como las 3 unides tiene viajes donde le “Falta”
    entregar producto el mas propenso a esto es en los camiones grandes,
    si se aumenta la cantidad a 7 reduciria los viajes “Faltantes”, otra
    solucion seria comprar camiones mas grandes.

<!-- end list -->

``` r
CamionPeque <- filter(data, UNIDAD == "Camion Pequeño")
sprintf('Ingresos Camiones Pequeños %f', sum(CamionPeque$Q))
```

    ## [1] "Ingresos Camiones Pequeños 112815.250000"

``` r
Panel <- filter(data, UNIDAD == "Panel")
sprintf('Ingresos Panel %f', sum(Panel$Q))
```

    ## [1] "Ingresos Panel 30566.500000"

``` r
CamionGrande <- filter(data, UNIDAD == "Camion Grande")
sprintf('Ingresos Camiones Grandes %f', sum(CamionGrande$Q))
```

    ## [1] "Ingresos Camiones Grandes 455466.500000"

``` r
for(i in 1:length(Faltantes)){
  faltantePanel <- filter(data, CLIENTE == Faltantes[i], UNIDAD == "Panel" )
  
}
for(i in 1:length(Faltantes)){
  faltantesPeque <- filter(data, CLIENTE == Faltantes[i], UNIDAD == "Camion Pequeño" )
}
for(i in 1:length(Faltantes)){
  faltantesGrande <- filter(data, CLIENTE == Faltantes[i], UNIDAD == "Camion Grande" )
}

sprintf('Cantidad de viajes faltantes con paneles %i', nrow(faltantePanel))
```

    ## [1] "Cantidad de viajes faltantes con paneles 18"

``` r
sprintf('Cantidad de viajes faltantes con camion pequeño %i', nrow(faltantesPeque))
```

    ## [1] "Cantidad de viajes faltantes con camion pequeño 44"

``` r
sprintf('Cantidad de viajes faltantes con camiones grande %i', nrow(faltantePanel))
```

    ## [1] "Cantidad de viajes faltantes con camiones grande 18"

``` r
sprintf('Cantidad de viajes faltantes con panel %f', sum(faltantePanel$Q))
```

    ## [1] "Cantidad de viajes faltantes con panel 1556.500000"

``` r
sprintf('Cantidad de viajes faltantes con camiones pequeño %f', sum(faltantesPeque$Q))
```

    ## [1] "Cantidad de viajes faltantes con camiones pequeño 8258.500000"

``` r
sprintf('Cantidad de viajes faltantes con camiones grande %f', sum(faltantesGrande$Q))
```

    ## [1] "Cantidad de viajes faltantes con camiones grande 25709.000000"

``` r
sprintf('Cantidad de Producto en camiones paneles %f', sum(faltantePanel$CANTIDAD))
```

    ## [1] "Cantidad de Producto en camiones paneles 6226.000000"

``` r
sprintf('Cantidad de Producto faltante en camiones pequeño %f', sum(faltantesPeque$Q))
```

    ## [1] "Cantidad de Producto faltante en camiones pequeño 8258.500000"

``` r
sprintf('Cantidad de Producto faltante en camiones grandes %f', sum(faltantesPeque$Q))
```

    ## [1] "Cantidad de Producto faltante en camiones grandes 8258.500000"

## Las tarifas actuales ¿son aceptables por el cliente?

  - Como podemos ver por la cantidad de viajes por rango de precio de
    0-100, se mantiene en promedio de 460 en todo los rangos, por lo
    cual se concluye que todas las tarifas son aceptables ya que
    mantienen un flujo casi constante.

<!-- end list -->

``` r
e1 <- filter(data, Q > 0, Q <= 100 )
nrow(e1)
```

    ## [1] 263

``` r
e2 <- filter(data, Q > 100, Q <= 200 )
nrow(e2)
```

    ## [1] 459

``` r
e3 <- filter(data, Q > 200, Q <= 300 )
nrow(e3)
```

    ## [1] 491

``` r
e4 <- filter(data, Q > 300, Q <= 400 )
nrow(e4)
```

    ## [1] 475

``` r
e5 <- filter(data, Q > 400, Q <= 500 )
nrow(e5)
```

    ## [1] 492

``` r
e6 <- filter(data , CREDITO == 30)
```

## ¿Nos están robando los pilotos?

  - No, no estan robando, esto se comprobo sacando el precio por Unidad
    buscando si en algun caso este variaba se comprobo que no vario.

<!-- end list -->

``` r
PrecioPorUnidad <- data$Q/data$CANTIDAD
# Ver si hay alguno que no cuadre
robo <- unique(data$Q/data$CANTIDAD)
"El unico valor es "
```

    ## [1] "El unico valor es "

``` r
robo
```

    ## [1] 0.25

\#\#¿Qué estrategias debo seguir? \* Comprar y cotizar camiones mas
grandes para reducir la cantidad de viajes faltantes \* Distribuir
acorde a la cantidad de productos que se distrubuye. \* Ya que la
cantidad de viajes por piloto en un dia son alrededor de 1 o 2, se
podria considerar reducir a los pilitos de tal manera que tenga de 4 a 5
viajes diarios, o reubicarlos en otra area donde sean más productivos.
\* Investigar la devolucion del cliente Gaño negro ya que se reportan
119 viajes devueltos con un total de 33070.50

## 80-20 de clientes y cuáles de ellos son los más importantes

  - Nuestro mejor cliente
  - TAQUERIA EL CHINITO con ingreso de 69135 representa el 11.54% de
    nuestro ingresos anuales
  - UBIQUO LABS con ingresos de 64250 representa el 10.7%
  - EL PINCHE OBELISCO con ingresos 71079 representa el 11.87%

<!-- end list -->

``` r
for(i in 1:length(Clientes)){
  t <- filter(data, CLIENTE == Clientes[i])
  vector <- c(Clientes[i],sum(t$Q))
  print(vector)
  
  IngresoTotal <- sum(data$Q)
  obelisto <- 35555 + 35524
  chinito <- 30861.5 + 38274
  ubico <- 3212 + 32125.75
  PorcentajeObelisco <- (38274*100)/IngresoTotal
  Porcentajechinito <- (37889.25*100)/IngresoTotal
  Porcentajeubico <- (37129*100)/IngresoTotal
  
}
```

    ## [1] "EL PINCHE OBELISCO / Despacho a cliente"
    ## [2] "35555"                                  
    ## [1] "TAQUERIA EL CHINITO |||Faltante" "30861.5"                        
    ## [1] "TIENDA LA BENDICION / Despacho a cliente"
    ## [2] "35338"                                   
    ## [1] "TAQUERIA EL CHINITO" "38274"              
    ## [1] "CHICHARRONERIA EL RICO COLESTEROL |||Faltante"
    ## [2] "32456.25"                                     
    ## [1] "UBIQUO LABS |||FALTANTE" "32125"                  
    ## [1] "EL GALLO NEGRO |||DEVOLUCION" "33070.5"                     
    ## [1] "EL GALLO NEGRO / Despacho a cliente" "34485"                              
    ## [1] "UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente"
    ## [2] "37889.25"                                          
    ## [1] "POLLO PINULITO|||FALTANTE" "33005.5"                  
    ## [1] "SPORTA, S.A./Despacho a cliente |||Faltante"
    ## [2] "34984.25"                                   
    ## [1] "HOSPITAL ROOSEVELT / Despacho a cliente"
    ## [2] "26275.75"                               
    ## [1] "UBIQUO LABS" "32125.75"   
    ## [1] "BAR LA OFICINA" "35163.5"       
    ## [1] "ABARROTERIA EBENEZER/Despacho a cliente"
    ## [2] "37129"                                  
    ## [1] "EL PINCHE OBELISCO |||Faltante" "35524"                         
    ## [1] "POLLO PINULITO/Despacho a cliente" "32100"                            
    ## [1] "HOSPITAL LAS AMERICAS" "22486"

## Mejores pilotos

  - Mejor piloto Fernando Mariano Berrio con 267 anuales
  - 2do mejor piloto Pedro Alvarez Parejo 253 anuales
  - 3ro mejor piloto Hector Aragones Frutos 248 anuales

## Transportes más efectivos

  - Camion Grande representa el 76.05 de nuestras entregas
  - Camion Camion Pequeño el 18.83 de nuestras entregas
  - Camion Panel el 5.10 de nuestras entregas

<!-- end list -->

``` r
DFPilotos <- function(){
  Piloto <- as.character(unique(data[["PILOTO"]]))
  df <- data.frame(
    Piloto = character(),
    Viajes_Anuales = character(),
    stringsAsFactors=FALSE
  )
  for(i in 1:length(Piloto)){
    nombre <- Piloto[i];
    CViajesAnuales <- as.character(nrow(filter(data, PILOTO == Piloto[i])))
    vector <- c(nombre,CViajesAnuales)
    print(vector)
  }
}
"Pilotos"
```

    ## [1] "Pilotos"

``` r
DFPilotos()
```

    ## [1] "Fernando Mariano Berrio" "267"                    
    ## [1] "Hector Aragones Frutos" "248"                   
    ## [1] "Pedro Alvarez Parejo" "253"                 
    ## [1] "Angel Valdez Alegria" "235"                 
    ## [1] "Juan Francisco Portillo Gomez" "212"                          
    ## [1] "Luis Jaime Urbano" "246"              
    ## [1] "Ismael Rodero Monteagudo" "244"                     
    ## [1] "Felipe Villatoro" "247"             
    ## [1] "Hector Giron" "228"

``` r
t <- filter(data, UNIDAD == "Panel" )
"Panel"
```

    ## [1] "Panel"

``` r
sum(t$Q)
```

    ## [1] 30566.5

``` r
Por <- (sum(t$Q)*100)/IngresoTotal
a <- filter(data, UNIDAD == "Camion Pequeño" )
"Camion Pequeño"
```

    ## [1] "Camion Pequeño"

``` r
sum(a$Q)
```

    ## [1] 112815.2

``` r
Por1 <- (sum(a$Q)*100)/IngresoTotal
s <- filter(data, UNIDAD == "Camion Grande" )
"Camion Grande"
```

    ## [1] "Camion Grande"

``` r
sum(s$Q)
```

    ## [1] 455466.5

``` r
Por2 <- (sum(s$Q)*100)/IngresoTotal
```
