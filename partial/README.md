dw-2020-parcial-1
================
Alex Palencia 201660391
9/3/2020

# Examen parcial

Indicaciones generales:

  - Usted tiene el período de la clase para resolver el examen parcial.

  - La entrega del parcial, al igual que las tareas, es por medio de su
    cuenta de github, pegando el link en el portal de MiU.

  - Pueden hacer uso del material del curso e internet (stackoverflow,
    etc.). Sin embargo, si encontramos algún indicio de copia, se
    anulará el exámen para los estudiantes involucrados. Por lo tanto,
    aconsejamos no compartir las agregaciones que generen.

## Sección I: Preguntas teóricas.

  - Existen 10 preguntas directas en este Rmarkdown, de las cuales usted
    deberá responder 5. Las 5 a responder estarán determinadas por un
    muestreo aleatorio basado en su número de carné.

  - Ingrese su número de carné en `set.seed()` y corra el chunk de R
    para determinar cuáles preguntas debe responder.

<!-- end list -->

``` r
set.seed("20160391") 
v<- 1:10
preguntas <-sort(sample(v, size = 5, replace = FALSE ))

paste0("Mis preguntas a resolver son: ",paste0(preguntas,collapse = ", "))
```

    ## [1] "Mis preguntas a resolver son: 1, 3, 5, 8, 9"

### Listado de preguntas teóricas

1.  Para las siguientes sentencias de `base R`, liste su contraparte de
    `dplyr`:
      - `str()` –\> Se pueden usar la funcion summarise\_all de dplyr
        dentro de esta se le pude pasar la funcion
        summarise\_all(n\_distinct)
      - `df[,c("a","b")]` –\> estamos seleccionando las columnas por lo
        que podemos utilizar la funcion Select de df %\>%
        dplyr::select(a,b)
      - `names(df)[4] <- "new_name"` donde la posición 4 corresponde a
        la variable `old_name` –\> Podemos utilizar la funcion rename
        para renombrar la tabla rename(df, ‘old\_name’ = ‘new\_name’)
      - `df[df$variable == "valor",]` Podemos utilizar la funcion filter
        filter(df, variable == “valor”)
2.  
3.  ¿Por qué en R utilizamos funciones de la familia apply
    (lapply,vapply) en lugar de utilizar ciclos?

> Porque R trabaja con vectores es decir un chunk de data con las
> funciones de lapply y vapply permite aplicar una funcion a todo este
> chunk de informacion, en R no es recomendado utilizar ciclos debido a
> que es lento y se tendria que itererar la cantidad de datos que quiero
> manejar.

4.  
5.  ¿Cuál es la forma correcta de cargar un archivo de texto donde el
    delimitador es `:`?

> Para leer un archivo con cualquier delimitador se utiliza read.delim
> para leer un archivo de texto con el delimator : seria : \* df \<-
> read.delim(file = “nombre.txt”, header = TRUE, sep = “:”)

6.  
7.  
8.  Si en un dataframe, a una variable de tipo `factor` le agrego un
    nuevo elemento que *no se encuentra en los niveles existentes*,
    ¿cuál sería el resultado esperado y por qué?
      - El nuevo elemento
      - `NA`

> El resultado esperado sera NA, ya que los niveles de un factor
> Delimitan a que este factor solo contenga esos valores

9.  En SQL, ¿para qué utilizamos el keyword `HAVING`?

> Se utliza para filtrar con un condicional la data que agrupamos con
> GROUP BY la sintaxis es la siguiente \* SELECT COL1 FROM WHERE
> CONDITION GROUP BY COL1 HAVING CONDITION

10. 
Extra: ¿Cuántos posibles exámenes de 5 preguntas se pueden realizar
utilizando como banco las diez acá presentadas? (responder con código de
R.)

``` r
library(gtools)
N <- 10  # Número de elementos
n <- 5 # grupos de 5
 
preguntas <- c(1:N) # Son los alumnos con id un número consecutivo

combinaciones <- combinations(N, n, preguntas)
length(combinaciones)
```

    ## [1] 1260

### 1260 Combanaciones distintas Importando el orden

## Sección II Preguntas prácticas.

  - Conteste las siguientes preguntas utilizando sus conocimientos de R.
    Adjunte el código que utilizó para llegar a sus conclusiones en un
    chunk del markdown.

A. De los clientes que están en más de un país,¿cuál cree que es el más
rentable y por qué?

B. Estrategia de negocio ha decidido que ya no operará en aquellos
territorios cuyas pérdidas sean “considerables”. Bajo su criterio,
¿cuáles son estos territorios y por qué ya no debemos operar ahí?

### I. Preguntas teóricas

## A

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
data <- readRDS("D:/Documents/Data Wrangling/Data-Wrangling-UFM/partial/parcial_anonimo.rds")
# resumen data

data %>% summarise_all(n_distinct)
```

    ##   DATE Codigo Material Descripcion Pais Distribuidor Territorio Cliente Marca
    ## 1   28              64          64    2            4        104    2147    19
    ##   Canal Venta Unidades plaza Venta
    ## 1           4            317  3166

``` r
# unique(data$Pais)
# Codigo dos paises 4f03bd9b 4046ee34
Pais1 <- filter(data, Pais == "4046ee34")
Pais2 <- filter(data, Pais == "4f03bd9b")

y <- Pais1 %>% group_by(Cliente) %>% 
  summarize(SumVentas = sum(Venta)) %>%
  arrange(desc(SumVentas))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
x <- Pais2 %>% group_by(Cliente) %>% 
  summarize(SumVentas2 = sum(Venta)) %>%
  arrange(desc(SumVentas2))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
joined_df <- merge(x, y, by.x = "Cliente", 
             by.y = "Cliente")

joined_df$totalVentas <- joined_df$SumVentas2 + joined_df$SumVentas
joined_df %>% arrange(desc(totalVentas))
```

    ##    Cliente SumVentas2 SumVentas totalVentas
    ## 1 a17a7558   17368.25   2449.45    19817.70
    ## 2 ff122c3f    1085.26  14273.28    15358.54
    ## 3 c53868a0    6615.90   7196.97    13812.87
    ## 4 044118d4    6627.23   2809.01     9436.24
    ## 5 f676043b    2846.92    787.83     3634.75
    ## 6 f2aab44e     360.64     39.75      400.39
    ## 7 bf1e94e9       0.00      0.00        0.00

### El cliente más rentable que se encuentra en los paises es el Cliente a17a7558 ya que podemos ver que es el cliente que más vende en ambos paises, en la tabla podemos ver en orden a los clientes más rentables

## B

``` r
###

Perdidas <- filter(data, Venta <= 0)
groupTerr <- Perdidas %>% group_by(Territorio) %>%
  summarize(SumPerdidas = sum(Venta)) %>%
  arrange((SumPerdidas))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
groupTerr
```

    ## # A tibble: 104 x 2
    ##    Territorio SumPerdidas
    ##    <chr>            <dbl>
    ##  1 f7dfc635       -14985.
    ##  2 77192d63        -5641.
    ##  3 72520ba2        -3761.
    ##  4 69c1b705        -3370.
    ##  5 1d407777        -3300.
    ##  6 bc8e06ed        -3269.
    ##  7 2e812869        -3056.
    ##  8 67e9cc18        -2721.
    ##  9 8f79b7f8        -1858.
    ## 10 a0d39798        -1779.
    ## # ... with 94 more rows

### Como podemos observar en la tabla el territorio f7dfc635 se reportan una perdida de 14985.02 por lo cual recomiendo que se analise el porque de estas perdidas en este territorio o salirse del territorio y enfocarse en otro territorio
