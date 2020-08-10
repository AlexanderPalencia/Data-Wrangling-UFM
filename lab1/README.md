Lab 1
================
Alexander Palencia 20160391
8/9/2020

## Problema 1

El archivo nuevo genrado es MyData.csv

``` r
library(readxl)


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
head(df)
```

    ## # A tibble: 6 x 9
    ##   COD_VIAJE CLIENTE      UBICACION CANTIDAD PILOTO        Q CREDITO UNIDAD Fecha
    ##       <dbl> <chr>            <dbl>    <dbl> <chr>     <dbl>   <dbl> <chr>  <chr>
    ## 1  10000001 EL PINCHE O~     76002     1200 Fernando~ 300        30 Camio~ 01-2~
    ## 2  10000002 TAQUERIA EL~     76002     1433 Hector A~ 358.       90 Camio~ 01-2~
    ## 3  10000003 TIENDA LA B~     76002     1857 Pedro Al~ 464.       60 Camio~ 01-2~
    ## 4  10000004 TAQUERIA EL~     76002      339 Angel Va~  84.8      30 Panel  01-2~
    ## 5  10000005 CHICHARRONE~     76001     1644 Juan Fra~ 411        30 Camio~ 01-2~
    ## 6  10000006 UBIQUO LABS~     76001     1827 Luis Jai~ 457.       30 Camio~ 01-2~

## Problema 2

``` r
calculate_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

v1 <- c(2,1,2,3,1,2,3,4,1,5,5,3,2,3)
v2<- c(5,2,5,9,9,7,5,6,1,3,9,7,4,9)
v3 <- c(4,5,3,6,6,3,3,1,2,5,7,5,8,7)
list_vector <- list(v1,v2,v3)
lapply(list_vector, calculate_mode)
```

    ## [[1]]
    ## [1] 2
    ## 
    ## [[2]]
    ## [1] 9
    ## 
    ## [[3]]
    ## [1] 5

## Problema 3

``` r
data <- read.delim("./Data1/INE_PARQUE_VEHICULAR_080219.txt", header=TRUE, sep="|")
```

    ## Warning in scan(file = file, what = what, sep = sep, quote = quote, dec = dec, :
    ## EOF within quoted string

``` r
head(data)
```

    ##   ANIO_ALZA MES NOMBRE_DEPARTAMENTO NOMBRE_MUNICIPIO MODELO_VEHICULO
    ## 1      2007   5       HUEHUETENANGO    HUEHUETENANGO            2007
    ## 2      2007   5         EL PROGRESO        EL JICARO            2007
    ## 3      2007   5          SAN MARCOS             OCOS            2007
    ## 4      2007   5           ESCUINTLA         SAN JOSÉ            2006
    ## 5      2007   5             JUTIAPA           MOYUTA            2007
    ## 6      2007   5           GUATEMALA        FRAIJANES            1997
    ##            LINEA_VEHICULO TIPO_VEHICULO USO_VEHICULO MARCA_VEHICULO CANTIDAD  X
    ## 1                SPORT125          MOTO  MOTOCICLETA      ASIA HERO        1 NA
    ## 2 BT-50 DBL CAB 4X2 TURBO       PICK UP   PARTICULAR          MAZDA        1 NA
    ## 3                   JL125          MOTO  MOTOCICLETA         KINLON        1 NA
    ## 4               JL125T-15          MOTO  MOTOCICLETA        JIALING        1 NA
    ## 5                 JH100-2          MOTO  MOTOCICLETA        JIALING        1 NA
    ## 6  TACOMA XTRA CAB 4X4 V6       PICK UP   PARTICULAR         TOYOTA        1 NA
