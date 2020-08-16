# Lab 2 SQL Basics


```python
import numpy as np
import pandas as pd
from pandasql import *
heroes = pd.read_csv("heroes_information.csv")

pysqldf = lambda q: sqldf(q,globals())
```

### 1. what is the first name in heroes table?


```python
query = " SELECT * FROM heroes LIMIT 1"
pysqldf(query)
```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>id</th>
      <th>name</th>
      <th>Gender</th>
      <th>Eye color</th>
      <th>Race</th>
      <th>Hair color</th>
      <th>Height</th>
      <th>Publisher</th>
      <th>Skin color</th>
      <th>Alignment</th>
      <th>Weight</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>0</td>
      <td>A-Bomb</td>
      <td>Male</td>
      <td>yellow</td>
      <td>Human</td>
      <td>No Hair</td>
      <td>203.0</td>
      <td>Marvel Comics</td>
      <td>None</td>
      <td>good</td>
      <td>441.0</td>
    </tr>
  </tbody>
</table>
</div>



### 2. How many publisher exists?


```python
query = "SELECT COUNT(DISTINCT Publisher) FROM heroes"
pysqldf(query)
```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>COUNT(DISTINCT Publisher)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>24</td>
    </tr>
  </tbody>
</table>
</div>



### 3. How many superheroes are taller than 2 meters (200 cms)?


```python
query2 = "SELECT COUNT(*) FROM heroes WHERE Height > 200"
pysqldf(query2)
```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>COUNT(*)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>59</td>
    </tr>
  </tbody>
</table>
</div>



### 4. How many superheroes are human and are taller than 2 meters (200 cms)?


```python
query2 = "SELECT COUNT(*) FROM heroes WHERE Height > 200 AND race = 'Human'"
pysqldf(query2)
```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>COUNT(*)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>12</td>
    </tr>
  </tbody>
</table>
</div>



### 5. How many superheroes weigh more than 100 lbs and less than 200 lbs?


```python
query2 = "SELECT COUNT(*) FROM heroes WHERE Weight BETWEEN 100 AND 200"
pysqldf(query2)
```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>COUNT(*)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>98</td>
    </tr>
  </tbody>
</table>
</div>



### 6.How many superheroes have blue or red eyes?


```python
query2 = "SELECT COUNT(*) FROM heroes WHERE `Eye color` in ('red', 'blue')"
pysqldf(query2)
```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>COUNT(*)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>271</td>
    </tr>
  </tbody>
</table>
</div>



### 7. How many superheroes are Human, Mutant and have Green hair or are Vampires with black hair?


```python
query2 = "SELECT COUNT(*) FROM heroes WHERE (Race in ('Human', 'Mutant') AND `Hair color` = 'Green') OR (Race = 'Vampire' AND `Hair color` = 'Black')"
pysqldf(query2)
```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>COUNT(*)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>5</td>
    </tr>
  </tbody>
</table>
</div>



### 8. What is the name of the first superhero if I sort the table by race in order falling?


```python
query2 = "SELECT Name FROM heroes ORDER BY Race DESC LIMIT 1"
pysqldf(query2)
```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>name</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>Solomon Grundy</td>
    </tr>
  </tbody>
</table>
</div>



### 9. How many superheroes are male and how many female?


```python
query2 = "SELECT Gender, count(*) FROM heroes GROUP BY Gender"
pysqldf(query2)
```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>Gender</th>
      <th>count(*)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>None</td>
      <td>29</td>
    </tr>
    <tr>
      <th>1</th>
      <td>Female</td>
      <td>200</td>
    </tr>
    <tr>
      <th>2</th>
      <td>Male</td>
      <td>505</td>
    </tr>
  </tbody>
</table>
</div>



### 10. How many publishing houses have more than 15 superheroes?


```python
query2 = "SELECT Publisher, count(name) FROM heroes GROUP BY Publisher HAVING COUNT(Publisher) > 15"
pysqldf(query2)
```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>Publisher</th>
      <th>count(name)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>DC Comics</td>
      <td>215</td>
    </tr>
    <tr>
      <th>1</th>
      <td>Dark Horse Comics</td>
      <td>18</td>
    </tr>
    <tr>
      <th>2</th>
      <td>Marvel Comics</td>
      <td>388</td>
    </tr>
    <tr>
      <th>3</th>
      <td>NBC - Heroes</td>
      <td>19</td>
    </tr>
  </tbody>
</table>
</div>


