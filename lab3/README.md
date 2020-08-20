# LAB 3


```python
import sqlite3
import pandas as pd
conn = sqlite3.connect('sql-murder-mystery.db')
cursorObj = conn.cursor()
```

### Show tables of the db


```python
# cursorObj.execute("SELECT name FROM sqlite_master WHERE type='table'")
# rows = cursorObj.fetchall()

# for row in rows:
#     print(row)
    
    
tables = pd.read_sql("SELECT name FROM sqlite_master WHERE type='table'",conn)

print(tables)
```

                         name
    0      crime_scene_report
    1         drivers_license
    2                  person
    3  facebook_event_checkin
    4               interview
    5      get_fit_now_member
    6    get_fit_now_check_in
    7                  income
    8                solution
    

#### Describing tables of the db and their relationships


```python
print(cursorObj.execute("SELECT sql FROM sqlite_master WHERE name='crime_scene_report';").fetchall())
print(cursorObj.execute("SELECT sql FROM sqlite_master WHERE name='drivers_license';").fetchall())
print(cursorObj.execute("SELECT sql FROM sqlite_master WHERE name='person';").fetchall())
print(cursorObj.execute("SELECT sql FROM sqlite_master WHERE name='facebook_event_checkin';").fetchall())
print(cursorObj.execute("SELECT sql FROM sqlite_master WHERE name='interview';").fetchall())
print(cursorObj.execute("SELECT sql FROM sqlite_master WHERE name='get_fit_now_member';").fetchall())
print(cursorObj.execute("SELECT sql FROM sqlite_master WHERE name='get_fit_now_check_in';").fetchall())
print(cursorObj.execute("SELECT sql FROM sqlite_master WHERE name='income';").fetchall())
```

    [('CREATE TABLE crime_scene_report (\n        date integer,\n        type text,\n        description text,\n        city text\n    )',)]
    [('CREATE TABLE drivers_license (\n        id integer PRIMARY KEY,\n        age integer,\n        height integer,\n        eye_color text,\n        hair_color text,\n        gender text,\n        plate_number text,\n        car_make text,\n        car_model text\n    )',)]
    [('CREATE TABLE person (\n        id integer PRIMARY KEY,\n        name text,\n        license_id integer,\n        address_number integer,\n        address_street_name text,\n        ssn integer,\n        FOREIGN KEY (license_id) REFERENCES drivers_license(id)\n    )',)]
    [('CREATE TABLE facebook_event_checkin (\n        person_id integer,\n        event_id integer,\n        event_name text,\n        date integer,\n        FOREIGN KEY (person_id) REFERENCES person(id)\n    )',)]
    [('CREATE TABLE interview (\n        person_id integer,\n        transcript text,\n        FOREIGN KEY (person_id) REFERENCES person(id)\n    )',)]
    [('CREATE TABLE get_fit_now_member (\n        id text PRIMARY KEY,\n        person_id integer,\n        name text,\n        membership_start_date integer,\n        membership_status text,\n        FOREIGN KEY (person_id) REFERENCES person(id)\n    )',)]
    [('CREATE TABLE get_fit_now_check_in (\n        membership_id text,\n        check_in_date integer,\n        check_in_time integer,\n        check_out_time integer,\n        FOREIGN KEY (membership_id) REFERENCES get_fit_now_member(id)\n    )',)]
    [('CREATE TABLE income (\n        ssn integer PRIMARY KEY,\n        annual_income integer\n    )',)]
    

#### Select tables for examples


```python
tables = pd.read_sql("SELECT * FROM person LIMIT 1",conn)
print(tables)

tables = pd.read_sql("SELECT * FROM drivers_license LIMIT 1",conn)
print(tables)
```

          id                name  license_id  address_number address_street_name  \
    0  10000  Christoper Peteuil      993845             624        Bankhall Ave   
    
             ssn  
    0  747714076  
           id  age  height eye_color hair_color gender plate_number car_make  \
    0  100280   72      57     brown        red   male       P24L4U    Acura   
    
      car_model  
    0       MDX  
    

#### Finding the murder of January 15, 2018 in SQL City


```python
print(cursorObj.execute("SELECT description FROM crime_scene_report WHERE date = 20180115 AND type = 'murder' AND city = 'SQL City'").fetchall())
```

    [('Security footage shows that there were 2 witnesses. The first witness lives at the last house on "Northwestern Dr". The second witness, named Annabel, lives somewhere on "Franklin Ave".',)]
    

#### First witnesess: Morty Schapiro


```python
print(cursorObj.execute("SELECT MAX(address_number), * FROM person AS p INNER JOIN interview AS t ON p.id = t.person_id WHERE p.address_street_name = 'Northwestern Dr'").fetchall())
```

    [(4919, 14887, 'Morty Schapiro', 118009, 4919, 'Northwestern Dr', 111564949, 14887, 'I heard a gunshot and then saw a man run out. He had a "Get Fit Now Gym" bag. The membership number on the bag started with "48Z". Only gold members have those bags. The man got into a car with a plate that included "H42W".')]
    

#### Second witnesess: Annabel Miller


```python
print(cursorObj.execute("SELECT * FROM person AS p INNER JOIN interview AS t ON p.id = t.person_id WHERE p.name LIKE '%Annabel%' AND p.address_street_name = 'Franklin Ave'").fetchall())
```

    [(16371, 'Annabel Miller', 490173, 103, 'Franklin Ave', 318771143, 16371, 'I saw the murder happen, and I recognized the killer from my gym when I was working out last week on January the 9th.')]
    

### Guilty person 1: Jeremy Bowers id 67318


```python
print(cursorObj.execute("SELECT * FROM get_fit_now_member AS g " +
                        "INNER JOIN get_fit_now_check_in AS i ON g.id = i.membership_id " +
                        "INNER JOIN person AS p ON g.person_id = p.id " +
                        "INNER JOIN drivers_license AS d ON p.license_id = d.id " +
                        "WHERE g.membership_status = 'gold' AND " +
                        "i.membership_id LIKE '48Z%' AND " +
                        "i.check_in_date = 20180109 AND " +
                        "d.plate_number LIKE '%H42W%'").fetchall())
```

    [('48Z55', 67318, 'Jeremy Bowers', 20160101, 'gold', '48Z55', 20180109, 1530, 1700, 67318, 'Jeremy Bowers', 423327, 530, 'Washington Pl, Apt 3A', 871539279, 423327, 30, 70, 'brown', 'brown', 'male', '0H42W2', 'Chevrolet', 'Spark LS')]
    

### Guilty person 2: Miranda Priestly id 99716


```python
print(cursorObj.execute("SELECT * FROM interview WHERE person_id = 67318").fetchall())
```

    [(67318, 'I was hired by a woman with a lot of money. I don\'t know her name but I know she\'s around 5\'5" (65") or 5\'7" (67"). She has red hair and she drives a Tesla Model S. I know that she attended the SQL Symphony Concert 3 times in December 2017.\n')]
    


```python
print(cursorObj.execute("SELECT p.name, p.id, COUNT(*) FROM person as p INNER JOIN drivers_license as d ON p.license_id = d.id INNER JOIN facebook_event_checkin as f ON p.id = f.person_id WHERE d.height BETWEEN 65 AND 67 AND d.hair_color = 'red' AND d.car_make = 'Tesla' AND d.car_model = 'Model S' AND d.gender = 'female' AND f.event_name = 'SQL Symphony Concert' GROUP BY p.name").fetchall())
```

    [('Miranda Priestly', 99716, 3)]
    

### Checking my answer


```python
sql = "INSERT INTO solution VALUES (1, 'Jeremy Bowers')"
cursorObj.execute(sql)
conn.commit()
var = pd.read_sql("SELECT value FROM solution", conn)
var.iloc[0,0]
```




    "Congrats, you found the murderer! But wait, there's more... If you think you're up for a challenge, try querying the interview transcript of the murderer to find the real villian behind this crime. If you feel especially confident in your SQL skills, try to complete this final step with no more than 2 queries. Use this same INSERT statement with your new suspect to check your answer."




```python
sql = "INSERT INTO solution VALUES (1, 'Miranda Priestly')"
cursorObj.execute(sql)
conn.commit()
var = pd.read_sql("SELECT value FROM solution", conn)
var.iloc[0,0]
```




    'Congrats, you found the brains behind the murder! Everyone in SQL City hails you as the greatest SQL detective of all time. Time to break out the champagne!'


