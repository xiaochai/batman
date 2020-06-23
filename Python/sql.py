import sqlite3

conn = sqlite3.connect('my.db')
curs = conn.cursor()

try:
    curs.execute("""
    create table people(
        id int primary key,
        name string,
        age int
    )
    """)
except sqlite3.DatabaseError as e:
    print("create table:", e, "; but continue")

data = [
    [1, "Lee", 10],
    [2, "Lucy", 12],
    [3, "John", 9],
    [4, "Lily", 10],
    [5, "Green", 11]
]

# sqlite3.paramstyle为qmark，所以使用?为占位符号
sql =  "insert into people values(?,?,?)"

for row in data :
    try:
        curs.execute(sql, row)
    except sqlite3.DatabaseError as e:
        print("insert error:", e, data, ",oh..")


curs.execute("select * from people where age >= 11")
#print(list(curs.description))
# [('id', None, None, None, None, None, None), ('name', None, None, None, None, None, None), ('age', None, None, None, None, None, None)]
#print(list(curs.fetchall()))
# [('id', None, None, None, None, None, None), ('name', None, None, None, None, None, None), ('age', None, None, None, None, None, None)]

names = [f[0] for f in curs.description]
for row in curs.fetchall():
    for pair in zip(names, row):
        print('{}:{}'.format(*pair))
    print()
# id:2
# name:Lucy
# age:12
# 
# id:5
# name:Green
# age:11

import pymysql
conn = pymysql.connect("localhost","root","123456","test",charset='utf8')
curs = conn.cursor()

try:
    curs.execute("""
    create table people(
        id int primary key,
        name varchar(100),
        age int
    )
    """)
except pymysql.DatabaseError as e:
    print("create table:", e, "; but continue")

data = [
    [1, "Lee", 10],
    [2, "Lucy", 12],
    [3, "John", 9],
    [4, "Lily", 10],
    [5, "Green", 11]
]

# pymysql中的paramstyle为pyformat，而且所有的参数必须为字符串，即%s
sql =  "insert into people values(%s,%s,%s)"

for row in data :
    try:
        # 由于mysql需要将所有的参数转成字符串
        curs.execute(sql, row)
    except pymysql.DatabaseError as e:
        print("insert error:", e, row, ",oh..")


curs.execute("select * from people where age >= 11")
#print(list(curs.description))
# [('id', None, None, None, None, None, None), ('name', None, None, None, None, None, None), ('age', None, None, None, None, None, None)]
#print(list(curs.fetchall()))
# [('id', None, None, None, None, None, None), ('name', None, None, None, None, None, None), ('age', None, None, None, None, None, None)]

names = [f[0] for f in curs.description]
for row in curs.fetchall():
    for pair in zip(names, row):
        print('{}:{}'.format(*pair))
    print()
# id:2
# name:Lucy
# age:12
# 
# id:5
# name:Green
# age:11

# 一定要commit， 因为mysql支持回滚，所以如果没有commit，则之前的添加全部没有了
conn.commit()
conn.close()