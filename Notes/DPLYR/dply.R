library(tidyverse)
library(highcharter)
library(readr)


df <- read_delim("2010-2019-top.csv",
                  ";", escape_double = FALSE, trim_ws = TRUE )


view(df)


## Explorando la estructura de datos de un dataset
str(df)
glimpse(df)


# Renombrando columnas
names(df)[4] <- "top_genre"
names(df)


# dplyr: renombrando columnas
rename(df, top_genre = `top genre`)

# pipe operator %>%
df %>%
  select(artist, year) %>%
  head()

# rename_with
?rename_with

df <- rename_with(df, ~(gsub(" ", "_", .x)))

df <- df %>% rename_with(~tolower(gsub(" ", "_", .x)))


# select with base r
df$title
df[c(1, 2)]



# select dplyr
df %>%
  select(1,2) %>%
  head()

# quitando columnas en base r
df[-1]
df$X1 <- NULL


# quitando columnas con dplyr
df %>%
  select(-1) %>%
  head()

df %>%
  select(-X1) %>%
  head()


# transformar variables en base r
as.factor(df$title)

# dplyr mutate
?mutate_if

df <- mutate_if(df, is.character, as.factor)

# filter

df %>%
  select(artis, title, year) %>%
  filter(year == 2000 | year == 2011)

# summirese() & groupBy

# Cuantos artistas por año
df %>%
  select(year, artist) %>%
  group_by(year) %>%
  summarise(n = n())

# cuantos artistas tenemos
df %>%
  summarise(artistas_unicos = n_distinct(artist))

# Cuantos artistas distintos tenemos por a;o


df %>%
  select(year, artist) %>%
  group_by(year) %>%
  summarise(n = n_distinct(artist))


# Cuantas canciones distintas tenemos
df %>%
  summarise(canciones = n_distinct(title))

  ##### cuantos artistas tenemos?

df %>% 
  summarise(artistas_unicos = n_distinct(artist))

###### cuantos artistas distintos tenemos por año?
df %>% 
  select(year,artist) %>% 
  group_by(year) %>% 
  summarise(artistas_unicos = n_distinct(artist))


##### cuantas canciones distintas tenemos?

df %>% 
  summarise(canciones_unicas = n_distinct(title))


##### cuantas canciones aparecen mas de una vez?
df %>%  
  group_by(title) %>% 
  summarise(Canciones = n()) %>% 
  arrange(desc(Canciones))

df %>% select(title) %>% 
  summarise(canciones = sum(duplicated(df$title) == TRUE))


 df %>%  
   group_by(artist,title) %>% 
   summarise(Canciones = n()) %>% 
   arrange(desc(Canciones)) %>% 
   filter(Canciones > 1)





##### hay canciones de diferentes artistas que se llamen igual?
df %>%
  select(artist,title) %>% 
  group_by(title) %>% 
  summarise(canciones_ = n_distinct(artist)) %>% 
  filter(canciones_ > 1)

###### Del set de datos, que artistas han tenido mas de una ca ncion que fue popular en mas de un año?
df %>%
  group_by(artist,title) %>% 
  count() %>% 
  filter(n>1)



df %>%
  group_by(artist,title) %>% 
  count() %>% 
  filter(n>1) %>% 
  group_by(artist) %>% 
  summarise(artistas_canciones = n()) %>% 
  arrange(desc(artistas_canciones)) %>% 
  filter(artistas_canciones>1)
  

##### Highcharter
##### cuantos artistas distintos tenemos por año

df %>% 
  select(year,artist) %>% 
  group_by(year) %>% 
  summarise(n = n_distinct(artist)) %>% 
  hchart("column",hcaes(x = year, y = n)) %>% 
  hc_title(text = "Artistas distintos por año") %>% 
  hc_subtitle(text = "2019 tuvo la menor variedad, mientras que 2015 ha sido el año con mayor diversidad de artistas.")

  
df %>%
  select(artist,title) %>%
  group_by(artist) %>% 
  summarise(songs = n_distinct(title)) %>% 
  arrange(desc(songs)) %>%
  filter(songs > 5) %>% 
  hchart('column', hcaes(x = artist, y = songs))  %>% 
  hc_title(text = "Artistas con mayor cantidad de canciones populares ") %>% 
  hc_subtitle(text = " No puedo creer que The Chainsmokers tenga más de 10 canciones. ")































