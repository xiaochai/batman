## 快速上手

行结尾可加可不加分号

除法运算的结果为浮点数

执行整数除法可以使用//操作符

乘方运算符**

```
>>> 1//2
0
>>> 1/2
0.5
>>> (-2)**2
4
```

十六进制使用0x开头，八进制使用0o开头，二进制使用0b开头

```
>>> 0xF
15
>>> 0o10
8
>>> 0b10
2
```

Python中变量没有默认值，所以使用前必须赋值

input、print、pow、abs、round等内置函数使用

```
>>> x = input("The square of: ")
The square of: 10
>>> int(x) ** 2
100
>>> print("Hello world!")
Hello world!
>>> print(2**2)
4
>>> pow(2,2)
4
>>> abs(-199)
199
>>> round(123.12)
123
>>> round(123.72)
124

```

使用import导入模块

```
>>> import math
>>> math.sqrt(9)
3.0
>>> from math import sqrt
>>> sqrt(25)
5.0
```

python本身对复数有支持，cmath库对复数进行扩展

```
>>> 1j*1j
(-1+0j)
>>> import cmath
>>> cmath.sqrt(-1)
1j
```

海龟绘图小示例，使用turtle库（本身依赖于tkintery库）画一个正方形，其中forward前进n个像素点，left为逆时针旋转n度：

```python
from  turtle import *

forward(100)
left(90)
forward(100)
left(90)
forward(100)
left(90)
forward(100)

input("press enter to continue")
```


Python的注释为#开头

Python使用单引号和双引号都可以表示字符串，字符串拼接使用+号运算符，另外，直接把两个字符串放一起，也可以实现拼接

```
>>> a="Let's say " '"Hello world!"'
>>> a
'Let\'s say "Hello world!"'
```

repr函数配合print使用可保留原始字符串的样子：

```
>>> print(repr("Hello,\nworld"))
'Hello,\nworld'
>>> print(str("Hello,\nworld"))
Hello,
world
```

长字符串可以使用三引号（引号使用'和"都行），反斜杠换行，原始字符串表示（以r开头，注意最后一个字符不能是反斜杠）等表示：

```
>>> print('''That's great, like a "boss"!''')
That's great, like a "boss"!
>>> print("Hello,\
... world!")
Hello,world!
>>> print(r"C:\Program Files")
C:\Program Files
```

unicode的支持，以及编码的转换，以及使用b开头的字符串可以直接转化成bytes对象，以及可变bytes bytearray:

```
>>> print("\N{cat}\U0001F60A\u00C6")
🐈😊Æ

>>> "Hello".encode("ASCII")
b'Hello'
>>> "Hello".encode("UTF-8")
b'Hello'
>>> "Hello".encode("UTF-32")
b'\xff\xfe\x00\x00H\x00\x00\x00e\x00\x00\x00l\x00\x00\x00l\x00\x00\x00o\x00\x00\x00'

>>> "Hello\N{cat}".encode("UTF-8")
b'Hello\xf0\x9f\x90\x88'
>>> "Hello\N{cat}".encode("ASCII")
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
UnicodeEncodeError: 'ascii' codec can't encode character '\U0001f408' in position 5: ordinal not in range(128)
>>> "Hello\N{cat}".encode("ASCII", "replace")
b'Hello?'
>>> "Hello\N{cat}".encode("ASCII", "ignore")
b'Hello'

>>> x = bytearray(b'Hello')
>>> x[1] = ord(b'c')
>>> x
bytearray(b'Hcllo')

```

Python中默认使用UTF-8编码。


## 列表与元组

Python中的容器主要有序列（元组、列表等）、映射（字典等）、集合。

列表可修改，元组不可修改。

```
>>> edward = ['Edward Gumby', 42]
>>> john = ['John Smith', 50]
>>> database = [edward, john]
>>> database
[['Edward Gumby', 42], ['John Smith', 50]]
>>> ['rd']*2 + ['rs']
['rd', 'rd', 'rs']
```

序列的索引从0开始，可使用负数从最后往前数，-1表示最后一个元素。

字符串也是一个序列，Python中没有字符的概念，所以str[0]表示只有str第一个字符表示的字符串。

### 切片

```
>>> tag = '01234567'
>>> tag[1:3]
'12'
```

切片使用冒号来提取特定范围内的元素，冒号前后表示对应的索引，最终结果包含第一个索引，而不包含第二个索引。索引可以是负数，也可以省略，省略第一个表示从头开始，省略第二个表示直到最后一个元素（包含）；还可以使用步长来间隔获取元素，负数的步长表示从右往左提取：

```
>>> tag[1:-1]
'123456'
>>> tag[:]
'01234567'
>>> tag[:-1]
'0123456'
>>> tag[-3:-1]
'56'
>>> tag[-3:]
'567'

>>> tag[::2]
'0246'
>>> tag[-2::-2]
'6420'
```


### 其它操作

乘法操作可以复制序列元素，加法可以拼接两个序列，另外在Python中使用None关键字表示什么也没有。

in运算符用于判断指定元素是否在序列中，注意对于两个字符串使用in操作，表示的含义就变成检查子串了：

```
>>> 'ml' in ['mlh', 'abc']
False
>>> 'mlh' in ['mlh', 'abc']
True
>>> 'ml' in 'mlh'
True
```

获取长度，最小值，最大值：
```
>>> len([1,2,3])
3
>>> max(32,2,99)
99
>>> max([32,2,99])
99
>>> min([32,2,99])
2
```

### 列表


使用list类创建列表，列表是可以修改的，所以可以进行赋值，还有其它操作：


```
>>> a = list("Hello")
>>> a
['H', 'e', 'l', 'l', 'o']
>>> a[1] = 'a'
>>> ''.join(a)
'Hallo'
```

del a[1]: 删除元素

给切片赋值可以实现删除，插入，替换等多种功能

append：追加元素

clear清空列表

copy: 复制列表，如果正常赋值只是做一个关联，复制需要使用copy

count: 计算元素出现的个数

extend: 与加法拼接类似，只是会修改第一个列表值

index: 查找对应的元素，未找抛出异常，找到返回索引

insert: 插入对象到列表

pop: 从列表中删除参数指定的元素，默认为最后一个元素

remove: 删除第一个指定值的元素

reverse: 原地反转列表

sort: 原地排序

sorted: 并非list上的函数，而是将list做为参数传入，返回排序后的副本

以上的sort和sorted支持两个参数，key参数是一个函数，用于对元素生成一个关键字，按这个关键字排序，而reverse接受bool类型值，表示是否倒序排列。



```
>>> a = [1,2,3,4]
>>> a[1:]
[2, 3, 4]
>>> a[1:]  = [4,5,6,7]
>>> a
[1, 4, 5, 6, 7]
>>> a[1:3] = []
>>> a
[1, 6, 7]

>>> a.append(8)
>>> a
[1, 6, 7, 8]
>>> b = a.copy()
>>> a.clear()
>>> a
[]
>>> b
[1, 6, 7, 8]

>>> b.count(8)
1
>>> b.extend([8,9,10])
>>> b.count(8)
2

>>> b.index(8)
3
>>> b.pop(3)
8
>>> b.index(8)
3
>>> b.reverse()
>>> b
[10, 9, 8, 7, 6, 1]

>>> b.sort()
>>> b
[1, 6, 7, 8, 9, 10]
>>> sorted(b, reverse=True)
[10, 9, 8, 7, 6, 1]
```

### 元组

元组与列表类似，只是无法修改元组的值，元组的切片还是元组。tuple可以将序列转为元组。

```
>>> 1,2,3
(1, 2, 3)
>>> 1,
(1,)
>>> 3*(1+2,)
(3, 3, 3)
>>> x = 1,2,3
>>> x[0:1]
(1,)
>>> tuple('abc')
('a', 'b', 'c')
>>> tuple([1,2,3])
(1, 2, 3)
```

## 使用字符串

### 格式化输出

百分号运算符除了可以取模运算，还可以用于格式化输出字符串

```
>>> "Your name is %s, age :%d\n" % ("Lee", 12)
'Your name is Lee, age :12\n'
```

这里的%s与%d与C语言中的print函数类似，还可以使用%.3f这样的表示来获取指定位数的小数

还可以使用Template的方式来处理占位符的替换；或者使用{}这样的方式配合format函数使用，更加灵活（如果需要输出共括号时，使用两个花括号）；如果变量与替换字段同名，可以在字符串前加f轻松处理。

```
>>> from string import Template
>>> Template("Hello $name, welcome to $city").substitute(name="Lee", city="China")
'Hello Lee, welcome to China'


>>> "{3} {0} {2} {1} {3} {0}: {percent:.2f} }} {{ {arr[1]:03d}".format("be", "not", "or", "to", percent=0.324223432442343424, arr=[1,2,3])
'to be or not to be: 0.32 } { 002'
>>> "Hello {name} , welcome to {country}!".format_map({"name" : "Lee", "country":"China"})
'Hello Lee , welcome to China!'

>>> from math import pi
>>> f"pi is {pi:2.6f}"
'pi is 3.141593'
```

使用花括号的格式化还支持其它的语义，例如在变量后跟!s,!r, !a表示使用str、repr和ascii进行转换。更多的资料可以参考[这里](https://docs.python.org/3.4/library/string.html#format-string-syntax)

```
>>> "{str!s} {str!r} {str!a}".format(str="ok!\n")
"ok!\n 'ok!\\n' 'ok!\\n'"
```


### 字符串方法

string模块中定义了很多字符串的操作方法，但大部分在新版本的Python中都迁移到字符串方法上了，所以一般不再使用string中的方法，但有一些常用的常量：

```
>>> import string
>>> string.digits
'0123456789'
>>> string.ascii_letters
'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
>>> string.printable
'0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!"#$%&\'()*+,-./:;<=>?@[\\]^_`{|}~ \t\n\r\x0b\x0c'
>>> string.ascii_uppercase
'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
```

这里列举了一些字符串常用方法，更多的可以参考[这里](https://docs.python.org/3.4/library/stdtypes.html?highlight=center#string-methods)

center: 将字符串以给定长度居中显示

find: 在指定的范围内查到对应子串，返回位置，不存在返回-1，如果只需要判断是否包含字串，使用in操作符

join: 对字符串序列进行合并

split: 对字符串进行分割，可指定最多分割数量

lower: 字符串转小写

replace: 字符串替换

strip: 与php的trim一样，去掉头尾的空格或者指定字符

translate: 单字母替换指定字符，并可以删除指定字符

isspace/isdigit/isupper: 这一系列的is函数用于判断字符串是否满足指定规则

```
>>> "abc".center(11)
'    abc    '
>>> "abc".center(10, '-')
'---abc----'
>>> "abc".center(1, '-')
'abc'

>>> "i am ok ".find("i ")
0
>>> "i am ok ".find("i m")
-1
>>> "i am ok ".find("i", 3, 5)
-1
>>> "i am ok ".find("am", 1, 5)
2

>>> "+".join(["1","2"])
'1+2'
>>> "+".join(("1","2"))
'1+2'
>>> "+".join((1,2))
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: sequence item 0: expected str instance, int found
>>> "1 2 3 4".split(" ")
['1', '2', '3', '4']
>>> "1 2 3 4".split(" ",2)
['1', '2', '3 4']

>>> "i am ok".replace("ok", "bad")
'i am bad'

>>> table = str.maketrans('cs', 'kz', ' ')
>>> 'this is an incredible test'.translate(table)
'thizizaninkredibletezt'
```


## 字典

创建字典以及对应的操作，其中键的类型可以是任何不可变的类型，如字符串，元组，浮点数等

```
>>> {"Alice":10, "Bob":20}
{'Alice': 10, 'Bob': 20}
>>> dict([["Alice", "Bob"],[10,20]])
{'Alice': 'Bob', 10: 20}
>>> dict(Alice=10, Bob=20)
{'Alice': 10, 'Bob': 20}

>>> d =  dict(Alice=10, Bob=20)
>>> d["Alice"]
10
>>> d["Lee"] = 21
>>> d
{'Alice': 10, 'Bob': 20, 'Lee': 21}
>>> del d["Lee"]
>>> d
{'Alice': 10, 'Bob': 20}
>>> "Lee" in d
False
>>> "Bob" in d
True
```


常用的方法，更多查看[这里](https://docs.python.org/3.4/library/stdtypes.html?highlight=center#mapping-types-dict)：

clear：清空字典

copy: 复制字典

fromkeys: 创建并返回一个包含给定键的字典，可以设置默认值

get: 获取某一个键的值，与直接下标访问的区别是如果不存在，get会返回None，而下标访问会报错

items: 返回二元组的一个列表，包含了所以键值对，这个函数返回内容只是字典的另一个视图，不会进行复制


keys: 返回一个字典视图，其中包含指定字典中的键

pop: 删除指定key的元素，并返回，可指定没有这个元素时的默认值

popitem: 随机弹出一个键值项

setdefault: 如果某一个key不存在，则设置上某值

update: 更新值

values: 返回一个字典视图，其中包含指定字典中的值，可重复

```
>>> d =  dict(Alice=10, Bob=20)
>>> d.copy()
{'Alice': 10, 'Bob': 20}
>>> e = d.copy()
>>> d.clear()
>>> d
{}
>>> e
{'Alice': 10, 'Bob': 20}
>>> e.get('abc')
>>> e['abc']
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
KeyError: 'abc'

>>> e.fromkeys(["Lee","Chen"])
{'Lee': None, 'Chen': None}
>>> e
{'Alice': 10, 'Bob': 20}
>>> dict.fromkeys(["Lee","Chen"])
{'Lee': None, 'Chen': None}

>>> e.items()
dict_items([('Alice', 10), ('Bob', 20)])
>>> it = e.items()
>>> len(it)
2
>>> e["Bob"] = 21
>>> it
dict_items([('Alice', 10), ('Bob', 21)])

>>> e.keys()
dict_keys(['Alice', 'Bob'])
>>> e.pop("Lee", 22)
22

>>> e.popitem()
('Bob', 21)
>>> e
{'Alice': 10}
```

## 语句

print支持更多的参数，可以打印多个参数，并指定分隔符和结束符号，而import也可以为引入的包创建别名：

```
>>> print("Hello", "world", "my", "friend", sep = ', ', end=" -_- \n");
Hello, world, my, friend -_-

>>> import math as myMath
>>> myMath.sqrt(9)
3.0
>>> from math import sqrt as mySqrt
>>> mySqrt(9)
3.0
```

### 赋值

序列解包，可以使用\*来收集多余的值

```
>>> x,y,z = 1,2,3
>>> x,y = y,x
>>> x,y,z
(2, 1, 3)
>>> key,val = {"Bob":12, "Alice":11}.popitem()
>>> (key,val)
('Alice', 11)

>>> x,*y,z = 1,2
>>> x,y,z
(1, [], 2)
>>> x,*y,z = 1,2,3,4,5
>>> x,y,z
(1, [2, 3, 4], 5)
>>> x,*y,z = 1,2,5
>>> x,y,z
(1, [2], 5)
```


### 代码块

Python中使用冒号指示接下来是一个代码块，相同缩进的代码被认为同一个代码块，不相同则此代码块结束。


### 条件

在Python中这些值被认为是假```False None 0 "" () [] {}```

使用例子：

```python
num = int(input('Enter a number: '))
if num > 0:
    print('The number is positive')
elif num < 0:
    print('The number is negative')
else:
    print('The number is zero')
```

在Python中有一些特殊的条件，例如支持链式比较，is和is not 判断两对象是否为一个，使用and/or/not进行布尔运算（短路逻辑），assert断言直接在为假时让程序异常退出

```
>>> x=10
>>> 9 < x < 100
True

>>> x = y = [1,2]
>>> z = [1,2]
>>> x is y
True
>>> x is not z
True
>>> x ==  z
True

>>> assert x
>>> assert not x
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
AssertionError
>>> assert not x, x is not empty
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
NameError: name 'empty' is not defined
```


### 循环

```python
# 直到你输入了非空的名字之后才退出
name = ''
while not name:
    name = input('Please enter your name: ') 
print('Hello, {}!'.format(name))

# 每一行输出一个单词
words = ["this", "is", "a", "man"]
for word in words:
    print(word)

# 输出1到100
for number in range(1,101):
    print(number)

# 对于map的遍历
d = {'x': 1, 'y': 2, 'z': 3}
for key, value in d.items():
    print(key, 'corresponds to', value)
# 输出
# x corresponds to 1
# y corresponds to 2
# z corresponds to 3

# 对于同时需要遍历两个数组的情况，可以使用索引，也可以使用zip拼接成元组之后遍历
names = ['anne', 'beth', 'george', 'damon']
ages = [12, 45, 32, 102]
for i in range(len(names)):
    print(names[i], 'is', ages[i], 'years old')
for name, age in zip(names, ages):
    print(name, 'is', age, 'years old')
# 以上两个都输出
# anne is 12 years old
# beth is 45 years old
# george is 32 years old
# damon is 102 years old

# zip可以拼接做任意个数量，即使两个参数的数量不一致，则按最短的来
print(list(zip([1,2,3],range(0,100), [1,2])))
# 输出 [(1, 0, 1), (2, 1, 2)]

# enumerate可以将序列自动添加索引，在需要对序列遍历并需要索引的时候很管用
list(enumerate([1,2,3]))
# 输出 [(0, 1), (1, 2), (2, 3)]


# reversed返回一个倒序的视图，用于反向遍历
list(reversed([1,2,3]))
# 输出 [3, 2, 1]


# break和continue的用法与其它语言基本一致
while True:
    word = input('Please enter a word: ') 
    if not word:
        break
    print('The word was ', word)


# for可以配合else使用，只有当break没有执行到时，才会执行else下的内容
# 以下这个程序用于判断输入的数字是不是素数
n = int(input('enter a number: '))
for i in range(2,97):
    if n % i == 0:
        print("%d is a not prime number" % (n))
        break
else:
    print("%d is a prime number" % (n))

```


### 推导

推导类似于数学上的集合表示方式，与Haskell中的列表内包一致，例如```[x*x|x<-[0...10]]```表示0~10的平方列表，```[x|x<-[0...10],y%3==0]```表示0~10内可以被3整数的数，这些表达式使用Python的推导式表示如下：

```
>>> [x*x for x in range(0, 11)]
[0, 1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
>>> [x for x in range(0, 11) if x%3==0]
[0, 3, 6, 9]
>>> [(x,y) for x in range(0, 4) for y in range(100, 102)]
[(0, 100), (0, 101), (1, 100), (1, 101), (2, 100), (2, 101), (3, 100), (3, 101)]

>>> {x:"{} * {} = {}".format(x,x,x*x) for x in range(0,10)}
{0: '0 * 0 = 0', 1: '1 * 1 = 1', 2: '2 * 2 = 4', 3: '3 * 3 = 9', 4: '4 * 4 = 16', 5: '5 * 5 = 25', 6: '6 * 6 = 36', 7: '7 * 7 = 49', 8: '8 * 8 = 64', 9: '9 * 9 = 81'}
```

可以看到改成使用花括号就可以写出字典的生成器了


### 三个语句

pass: 空语句，什么也不做，用于占位，解决Python不支持空语句块的问题

del: 取消变量的引用指向，不并能真正释放值

exec: 将字符串当成Python脚本来执行，可以指定命名空间

eval: 与exec类似，执行语句，返回结果，而exec是不返回结果的


```
>>> from math import sqrt
>>> scope={}
>>> exec("sqrt=1", scope)
>>> sqrt(4)
2.0
>>> scope["sqrt"]
1

>>> scope = {}
>>> scope['x'] = 2
>>> scope['y'] = 3
>>> eval('x * y', scope)
6
>>> exec('x = 4', scope)
>>> eval('x * x', scope)
16
```

## 函数


```python
# 基本的函数定义，斐波那契数列
def fibs(num):
    'generate a list of Fibonacci'
    res = [0, 1]
    for i in range(num-2):
        res.append(res[i]+res[i+1])
    return res

print(fibs(10))
# 输出[0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

print(fibs.__doc__)
# 输出 generate a list of Fibonacci

help(fibs)
# 以下是输出
# Help on function fibs in module __main__:
# 
# fibs(num)
#     generate a list of Fibonacci


# 如果函数没有返回值，即空的return 语句，则实际会返回None
def noReturn():
    return
print(noReturn())
# 输出None

# 对于传入的参数如果是可变类型，则修改会影响参数的值，可以通过切片（切片生成的序列是复制的）来避免这一点
def modify(a):
    a[0] = 1
b = [0,1,2]
modify(b)
print(b)
# 输出[1,1,2]

# 如果传切片就不会有修改原值
b = [0,1,2]
modify(b[:])
print(b)
# 输出[0,1,2]

# 关键字参数与默认值，以及可变长参数
# 对于有默认值的参数，可以用关键字指定，带*号的变量表示会收集所有无关键字并且未匹配的参数，**表示收集关键字参数
def print_params(x, y, z=3, r=4, *pospar, **keypar): 
    print("x,y,z,r:", x, y, z, r, "\tpospar:", pospar, "\tkeypar", keypar)
# 有默认值的参数并非都要指定，可以跳过
print_params(1,2,r=9)
# 输出 x,y,z,r: 1 2 3 9        pospar: ()      keypar {}

# *和**指定的收集参数，都可以为空
print_params(1,2,z=3,r=4,k1=5,k2=6)
# 输出 x,y,z,r: 1 2 3 4        pospar: ()      keypar {'k1': 5, 'k2': 6}
print_params(1,2,3,4,5,6,7,8,k1=9,k2=10)
# 输出 x,y,z,r: 1 2 3 4        pospar: (5, 6, 7, 8)    keypar {'k1': 9, 'k2': 10}

# 以下调用报错，因为关键字参数必须放在最后
# print_params(1,2,z=100,r=32,433,k=1,k3=2)

# 参数分配，将序列或者字典分配到对应的参数中去
def add(x,y,z):
    return x+y+z
# 以下例子的结果都是6
add(1,2,3)
add(*(1,2,3))
add(*[1,2,3])
add(**{"x":1,"y":2,"z":3})

# 作用域
x = 10
y = 11
def g(y):
    global x
    x = x+1
    y = y+1
g(y)
# 可以看到x已经变了，但y却没有影响
print(x,y)

# 闭包
def f(x):
    def g(y):
        return x+y
    return g
# 每一次调用f()返回的函数，都包含了当时的x值
f1 = f(10)
f2 = f(11)
print(f1(1))
print(f2(1))

# 递归
# 阶乘
def factorial(n):
    if n == 1:
        return 1
    else:
        return n * factorial(n-1)
# 返回120
print(factorial(5))

# 幂
def power(x, n):
    if n == 0:
        return 1
    else:
        return x * power(x, n-1)
print(power(2,10))

# 二分查找
def search(sequence, number, lower = 0, upper = None):
    if upper is None:
        upper = len(sequence) - 1
    if lower > upper:
        return -1
    mid = int((lower+upper) / 2)
    if sequence[mid] == number:
        return mid
    elif sequence[mid] > number:
        return search(sequence, number, 0, mid-1)
    else:
        return search(sequence, number, mid+1, upper)
l = [1,2,3,4,5,6,7,8,9]
i = search(l, 4, 0, len(l)-1)
print(i) # 输出3
```

## 类

```python
# 类的创建和基本使用
# 这个self是约定命名，可以是任何变量名
class Person:
    def set_name(self, name):
        self.name = name
    def get_name(self):
        return self.name
    def greet(self):
        print("Hello, world! I'm {}.".format(self.name))
p1 = Person()
p1.set_name("Jonh")
p2 = Person()
p2.set_name("Lee")
p1.greet() # Hello, world! I'm Jonh.
p2.greet() # Hello, world! I'm Lee.
print(p1.get_name()) # Jonh

# 可以直接对属性进行设置
p1.name = "Mark"
print(p1.name) # Mark

# 类方法的调用，也可以使用类名的方式调用，我理解在类上的调用应该与此等价
Person.greet(p1) # Hello, world! I'm Mark.


# Python中没有私有属性，可以通过以下方法来达到私有属性的效果
class Sec :
    __name = "J" # 两个下划线开头的属性或者方法，实际上都会添加下划线开头加类名的前缀
    def set_name(self, name):
        self.__name = name
    def get_name(self):
        return self.__name
    def greet(self):
        print("Hello, world! I'm {}.".format(self.__name))

s = Sec()
s.greet() # Hello, world! I'm J.
s.__name = "K"
s.greet() # Hello, world! I'm J.
s._Sec__name = "K" # 双下划线开头的会添加前缀，实际中不推荐这样使用
s.greet() # Hello, world! I'm K.
# 也可以对一个不存在的属性赋值
s.__k  = "s"

# 单下划线也被视为不能直接修改约定，在使用from module import *中不会导入以下划线开头的名称

# 类名空间下的成员是所有对象共享的，也是独立于对象的，如果对象的属性没有定义，则使用类的属性
print(Sec._Sec__name) # J
# 所以类也可以被当成命名空间来使用，因为在类中可以直接执行语句
class C:
    print("Class C being defined")
# 以上输出 Class C being defined


# 继承的基本使用
class Base:
    def init(self):
        self.p = "base"
    def show(self):
        print("I'm ", self.p)

# 在定义类时添加括号表示继承的基类
class Extend(Base):
    def init(self):
        self.p = "extend"

b = Base()
b.init()
b.show() # I'm  base
e = Extend()
e.init()
e.show() # I'm  extend

# 与继承相关的方法
# 是否是继承关系
print(issubclass(Extend, Base)) # True
# 获取基类，由于支持多重继承，所以还一个属性__bases__
print(Extend.__base__) # <class '__main__.Base'>
# object是所有类的基类
print(Base.__base__) # <class 'object'>

# 以下两个都返回True，判断对象是否是某个类
print(isinstance(Extend(), Base))
print(isinstance(Extend(), Extend))
# 获取对象属于哪个类
print(Extend().__class__) # <class '__main__.Extend'>

# 多重继承
# 如果写成Super(Base, Extend)则无法运行，因为不满足MRO，可阅读参考文献
# 所以在多重继承中注意两个超类含有同一个属性或者方法的情况
class Super(Extend, Base): pass
s = Super()
s.init()
s.show() # I'm  extend


# 判断某一个对象是否有某一个属性，以及这个属性是否可调用
print(hasattr(s, "show")) # True
# getattr的第三个参数表示不存在时的默认值，如果不提供默认值且不存在，则报错
print(getattr(s, "p")) # extend
# callable检查是否可调用
print(callable(getattr(s, "show", None))) # True
# 设置对象的属性，返回None
setattr(s, "p1", "p1val")
# 获取所有属性
print(s.__dict__) # {'p': 'extend', 'p1': 'p1val'}

# 抽象基类
# 在Python中并没有提供原生的语法来支持抽象类，但可以使用abc这个官方模块解决此问题
from abc import ABC, abstractclassmethod
class Talker(ABC):
    @abstractclassmethod
    def talk(self): pass
# 以下语句会报错TypeError: Can't instantiate abstract class Talker with abstract methods talk
# t = Talker()
# 如果只是继承了而没有实现抽象方法，还是会报错
class Foo(Talker): pass
# t = Foo()
class Knigget(Talker):
    def talk(self):
        print("Ni")
k = Knigget()
# 一般的抽象类应用场景中会有isinstance判断
if isinstance(k, Talker):
    k.talk() # Ni
# 但这与Python的鸭子类型的编程思想，如果一个类有talk方法，但没有继承Talker，则在此场景中没有办法调用talk

# register提供了将某一类注册为另外一个类子类的办法
class Herring:
    def talk(self):
        print("Blue")
h = Herring()
print(isinstance(h, Talker)) # False
# 将Herring 注册为Talker的子类
Talker.register(Herring)
print(isinstance(h, Talker)) # True
print(issubclass(Herring, Talker)) # True

# 这种做法的问题是，如果Herring没有实现talk方法，则此时调用talk方法会直接报错，如下：
class Clam: pass
Talker.register(Clam)
c = Clam()
if isinstance(c, Talker):
    c.talk() # 这里将报错，因为Clam没有实现talk方法，但又是Talker的子类

```

## 异常

使用raise可以抛出异常

```
>>> raise Exception
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
Exception
>>> raise Exception("hyperdrive overload")
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
Exception: hyperdrive overload

>>> class MyException(Exception): pass
... 
>>> raise MyException("My")
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
__main__.MyException: My
```

Exception是几乎所有异常类的基类，raise可以直接跟类名，也可以跟对象。可以直接继承Exception来定义自己的异常类。

除了Exception异常类，还有以下常用的内置异常类：

AttributeError  ：引用属性或给它赋值失败时引发
OSError： 操作系统不能执行指定的任务(如打开文件)时引发，有多个子类 
IndexError ： 使用序列中不存在的索引时引发，为LookupError的子类
KeyError ： 使用映射中不存在的键时引发，为LookupError的子类
NameError ： 找不到名称(变量)时引发
SyntaxError ： 代码不正确时引发
TypeError ：将内置操作或函数用于类型不正确的对象时引发
ValueError ： 将内置操作或函数用于这样的对象时引发:其类型正确但包含的值不合适
ZeroDivisionError：除法或求模运算的第二个参数为零时引发


### 捕获异常

```python
# 异常的基本用法
try:
    x = int(input('Enter the first number: '))
    y = int(input('Enter the second number: ')) 
    print(x / y)
except ZeroDivisionError: # 如果y为0，则为ZeroDivisionError，走此流程
    print("The second number can't be zero!")
except (Exception, TypeError) as e: # 其它的Exception，走此流程，并捕获了异常的对象，将其打印出来，这里的TypeError多余，只是为了显示一次except可以捕获多个异常类型
    print("Other error occur: ", e)
except as e: # 其它非Exception的异常走此流程，例如Ctrl+C中止程序的异常（KeyboardInterrupt)，他的基类是BaseException（这类也是Exception的基类）
    print("not Exception occur")
else: # 如果没有异常，会执行此语句块
    print("Calculate complete")
finally: # 无论是否有异常，都会执行此语句块，一般用于做清理工作，例如连接的关闭等
    print("clear")
```

有时候需要捕获异常之后记录日志并重新抛出异常，可以使用无参数的raise：

```python
try: 1/0
except ZeroDivisionError as e:
    print("The second number can't be zero!")
    raise
# The second number can't be zero!
# Traceback (most recent call last):
#   File "test.py", line 1, in <module>
#     try: 1/0
# ZeroDivisionError: division by zero

try: 1/0
except ZeroDivisionError as e:
    print("The second number can't be zero!")
    raise ValueError
# 如果引发了别的异常，则在原先的异常会被存储在异常上下文中，在最终输出时体现出来
# 这里也可以写成raise ValueError from e
# The second number can't be zero!
# Traceback (most recent call last):
#   File "test.py", line 1, in <module>
#     try: 1/0
# ZeroDivisionError: division by zero
# 
# During handling of the above exception, another exception occurred:
# 
# Traceback (most recent call last):
#   File "test.py", line 4, in <module>
#     raise ValueError
# ValueError

try: 1/0
except ZeroDivisionError as e:
    print("The second number can't be zero!")
    raise ValueError from None
# 如果需要禁用上下文，可以使用from None
# The second number can't be zero!
# Traceback (most recent call last):
#   File "test.py", line 4, in <module>
#     raise ValueError from None
# ValueError
```


### 警告

warning包提供了一些警告相关的工具，他们会在控制台输出警告信息而不打断程序的运行。此包也提供了对这些警告的控制，例如过滤某些警告，将某些警告上升为异常等等。更多内容可以参考[此](https://docs.python.org/3.4/library/warnings.html?highlight=filterwarnings)

```python
from warnings import warn,filterwarnings
warn("I've got a bad feeling about this.")
print("I'm ok")
# test.py:2: UserWarning: I've got a bad feeling about this.
#   warn("I've got a bad feeling about this.")
# I'm ok

filterwarnings("ignore")
warn("I've got a bad feeling about this.") # 不再输出

filterwarnings("error")
warn("I've got a bad feeling about this.") # 直接抛出异常
```

## 魔法方法、特性和迭代器

### 构造函数

``` python
class FooBar:
    # 构造函数
    def __init__(self, value = 42):
        self.somevar = value
    # 析构函数，在垃圾回收时被调用，但这个调用的时机很难把握
    def __del__(self):
        self.somevar = 1111
        print("del", self.somevar)
print(FooBar().somevar)
print(FooBar("hhh").somevar)
# 以下是这个输出，很奇怪的是析构函数的输出早于正常输出
# del 1111
# 42
# del 1111
# hhh

# 函数重写
class Bird:
    def __init__(self):
        self.hungry = True
    def eat(self):
        if self.hungry:
            print("Aaaah..")
        else:
            print("No, thanks")
class SongBird(Bird):
    def __init__(self):
        # 调用超类的构造函数方法，也可以使用Bird.__init__(self)，但使用super可以在有多个超类的情况下也能正确处理
        super().__init__()
        self.sound = "Squawk!"
    def sing(self):
        print(self.sound)

s = SongBird()
s.eat()
```

### 序列协议

```python

def check_index(key): 
    """
    指定的键是否是可接受的索引?
    键必须是非负整数，才是可接受的。如果不是整数， 将引发TypeError异常;如果是负数，将引发Index Error异常(因为这个序列的长度是无穷的)
    """
    if not isinstance(key, int): 
        raise TypeError 
    if key < 0: raise IndexError

class ArithmeticSequence:
    def __init__(self, start = 0, step = 1, max = 1000):
        """
        初使化这个算术序列

        start   -序列中的第一个值
        step    -两个相邻序列的差
        max     -最大值，包括此最大值 
        changed -一个字典，保存用户修改过的值
        """
        self.start = start
        self.step = step
        self.max = max
        self.changed = {}
    def __getitem__(self, key):
        """
        从序列中获取一个元素
        """
        check_index(key)

        try: return self.changed[key]
        except KeyError:
            return self.start + self.step * key
    def __setitem__(self, key, value):
        """
        修改算术序列中的元素
        """
        check_index(key)
        self.changed[key] = value

    def __len__(self):
        """
        返回序列的长度
        """
        return int((self.max-self.start)/self.step)
    def __delitem__(self, key):
        """
        删除对应的change值，返回默认值
        """
        check_index(key)
        del self.changed[key]


s = ArithmeticSequence(0, 1)
print(s[4], s[5]) # 依赖于__getitem__方法，输出 4 5
s[5] = 10 # 依赖于__setitem__方法，将值 设置到changed里
print(s[5], s[4]) # 输出10 4
print(len(s)) # 依赖于__len__方法，输出 1000
del s[5] # 依赖于__delitem__
print(s[5]) # 输出5
```

通过继承list来实现扩展列表的功能

```python
# 从list中继承来扩展列表
class CounterList(list):
    def __init__(self, *args):
        super().__init__(*args)
        self.counter = 0
    # 有一些操作访问的方法并不都调用__getitem__，所以像pop这一类的操作无法增加计数
    def __getitem__(self, index):
        self.counter += 1
        return super().__getitem__(index)

c = CounterList([0,1,2,3,4,5,6,7])
print(c[1] + c[2]) # 3
print(c.counter) # 2
```


更多的魔法方法可以参考[Special method names](https://docs.python.org/3/reference/datamodel.html?highlight=__len__#special-method-names)

### 特性（property）

一般的属性我们会为其设置get和set方法。通过property可以把一个虚拟的属性当成正常的属性来设置和获取

```python
class Rectangle:
    def __init__(self):
        self.width = 0
        self.height = 0
    def set_size(self, size):
        self.width, self.height = size
    def get_size(self):
        return (self.width, self.height)
    size = property(get_size, set_size)

r1 = Rectangle()
# 正常我们访问size时，是通过get和set方法来的
r1.set_size((10,20))
print(r1.get_size()) # (10, 20)
# 添加了size = property(get_size, set_size)之后，就可以直接给属性赋值和访问了
# property方法还支持可选的fdel，删除函数，doc文档 等参数
r1.size = (20, 40)
print(r1.size, r1.get_size()) # (20, 40) (20, 40)
```

property的实现原理是通过property类中的__get__、__set__、__delete__这些魔术方法来处理的。这些方法被定义为描述符协议，可以拦截对属性的访问，设置和删除。

除了使用property来实现对象属性的访问拦截，也可以通过以下这些方法来实现，相比函数property，这些魔法方法使用起来要棘手些(从某种程度上说，效率也更低)，但在同时处理多个特性时很有用：

> __getattribute__(self, name):在属性被访问时自动调用(只适用于新式类)。
> __getattr__(self, name):在属性被访问而对象没有这样的属性时自动调用。 
> __setattr__(self, name, value):试图给属性赋值时自动调用。 
> __delattr__(self, name):试图删除属性时自动调用。

```python
class Rectangle:
    def __init__(self):
        self.width = 0
        self.height = 0
    # 所有的设置都调用__setattr__，所以也要处理好非size的属性赋值，对__dict__的赋值不会再次调用__setattr__
    def __setattr__(self, name, value):
        if name == 'size':
            self.width, self.height = value
        else:
            self.__dict__[name] = value
    # 只有在没有找到对应属性的情况下才会调用__getattr__，所以非size的，都直接报错，这个错一定要是AttributeError才能在hasattr和getattr方法下正确处理
    def __getattr__(self, name):
        if name == "size":
            return (self.width, self.height)
        else:
            raise AttributeError()
    # 不使用__getattr__也可以使用__getattribute__方法，注意这个方法是代理所有的访问，包括__dict__的问题，所以需要调用到超类的此方法
    # def __getattribute__(self, name):
    #     if name == "size":
    #         return (self.width, self.height)
    #     else:
    #         return super().__getattribute__(name)

r = Rectangle()
r.size = (10, 20)
r.a = 100
print(r.size, r.a, r.width) # (10, 20) 100 10
```



### 类方法和静态方法

```python
# 静态方法与类方法
class MyClass:
    # 这个注解也可以换成使用这一语句smeth = staticmethod(smeth)
    @staticmethod
    def smeth():
        print('This is a static method')

    # 这个注解也可以换成使用这一语句cmeth = classmethod(smeth)
    @classmethod
    def cmeth(cls):
        print('This is a class method of ', cls)

MyClass.smeth() # This is a static method
MyClass.cmeth() # This is a class method of  <class '__main__.MyClass'>
```

### 迭代器

实现了__iter__的对象可以使用for来迭代。__iter__方法返回包含有__next__方法的迭代器对象，这个__next__方法没有参数，返回下一个值，如果没有值了，需要引发StopIteration异常


```python
class Fibs:
    def __init__(self, max = 1000):
        self.a = 0
        self.b = 1
        self.max = max
    def __next__(self):
        self.a, self.b = self.b, self.a + self.b
        if self.b > self.max:
            raise StopIteration()
        return self.b
    def __iter__(self):
        return self

for i in Fibs():
    print(i)
print(list(Fibs())) # [1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987]
```

### 生成器

```python
# 包含yield语句的函数被称之为生成器，生成器可以生成多个值，每一次使用yield生成一个值之后就被冻结，等待被重新唤醒。
# 调用生成器的函数，返回了生成器的迭代器，可以像使用普通迭代一样来使用生成器的迭代器
def p(max):
    for i in range(max):
        yield i**2
gp = p(10)
print(gp) # <generator object p at 0x10d486550>
print(next(gp), next(gp), next(gp), next(gp), next(gp), next(gp)) # 0 1 4 9 16 25
# 也可以使用类似于列表推导的方式（将中括号换成小括号）来创建生成器
print((i**2 for i in range(10))) # <generator object <genexpr> at 0x10d5584d0>
# 如果直接既有的小括号内使用生成器推导，则可以省掉生成器使用的小括号，如下：
print(i**2 for i in range(10)) # <generator object <genexpr> at 0x10d5584d0>

# 将一个n层的嵌套列表，展开成一级列表
def flatten(nested):
    try:
        # 字符串即使单个字符也可以迭代，所以需要对字符串做特殊处理
        try: nested + ''
        except TypeError: pass
        else: raise TypeError
        for sublist in nested:
            for element in flatten(sublist):
                yield element
    except TypeError:
        yield nested
print(list(flatten([[[1], 2], 3, 4, [5, [6, 7]], 8]))) # [1, 2, 3, 4, 5, 6, 7, 8]
print(list(flatten(['foo', ['bar', ['baz']]]))) # ['foo', 'bar', 'baz']

# 外部可以通过生成器的迭代器的send方法往生成器中发送数据，体现出来的就是yield的返回值，这与next的区别就是next唤醒的yield，返回值为None
def seqWithSkip(begin):
    i = begin
    while True:
        skip = (yield i)
        if skip is not None: i+=skip
        i+=1
sws = seqWithSkip(100)
print(next(sws), next(sws), next(sws)) # 100 101 102
print(sws.send(100), next(sws), next(sws)) # 203 204 205
```

### 八皇后问题

```python
# 八皇后问题，state为已经确认的前几行皇后的位置，判断下一行的位置nextX是否会冲突
# 冲突的定义是有两个皇后在同一列，或者在同一行或者在对角线上
def conflict(state, nextX):
    nextY = len(state)
    for i in range(nextY):
        if state[i] == nextX or abs(state[i] - nextX) == abs(i - nextY):
            return True
    return False

# 使用递归的方式来解题，queens函数根据皇后数量（棋盘的大小）和给出的状态，补全剩下的状态
# 下一个状态肯定是从0~num中选一个，而且必须满足条件的，假设取出了pos
# 如果这个pos是最后一个（这也是递归的终结条件），则直接返回(pos,)此元组
# 如果非最后一个，则将此位置放入state中，递归求剩下的状态，返回的满足条件的元组再补上pos就是最终要求的状态了
def queens(num = 8, state = ()):
    for pos in range(num):
        if not conflict(state, pos):
            if len(state) == num - 1: 
                yield (pos,)
            else:
                for result in queens(num, state + (pos,)):
                    yield (pos,) + result

print(list(queens(4))) # [(1, 3, 0, 2), (2, 0, 3, 1)]
print(list(queens(8)))
```

## 模块

可以使用sys.path.append添加模块的查找路径，注意模块只会导入一次，可以使用importlib模块的importlib.reload(queens)来重新导入。

```
>>> import sys
>>> sys.path
['', '/usr/local/Cellar/python/3.7.7/Frameworks/Python.framework/Versions/3.7/lib/python37.zip', '/usr/local/Cellar/python/3.7.7/Frameworks/Python.framework/Versions/3.7/lib/python3.7', '/usr/local/Cellar/python/3.7.7/Frameworks/Python.framework/Versions/3.7/lib/python3.7/lib-dynload', '/Users/liqingshou/Library/Python/3.7/lib/python/site-packages', '/usr/local/lib/python3.7/site-packages', './']
>>> sys.path.append("./")
>>> import queens
>>> list(queens.queens(4))
[(1, 3, 0, 2), (2, 0, 3, 1)]

>>> import importlib
>>> importlib.reload(queens)
```

模块的测试很重要，可以在模块中添加测试代码。为了不在正常导入模块时运行测试代码，可以使用以下方式``` if __name__ == '__main__': test()```，这样就不会在导入的时候运行了。

更专业的测试代码应该使用独立的程序来处理。

除修改sys.path的方法来改变模块的搜索目录外，还可以通过环境变量的方式：

```shell
$ PYTHONPATH=$PYTHOPATH:./ python3
Python 3.7.7 (default, Mar 10 2020, 15:43:33) 
[Clang 11.0.0 (clang-1100.0.33.17)] on darwin
Type "help", "copyright", "credits" or "license" for more information.
>>> import queens
>>> list(queens.queens(4))
[(1, 3, 0, 2), (2, 0, 3, 1)]

```

在某一个目录当中添加__init__.py文件，则这个目录可以被识别为一个包，可以像导入模块一样导入包。

例如目录结构为:

```
drawing
├── __init__.py
├── colors.py
└── shapes.py
```

则可以使用以下语句：

> import drawing： 可使用__init__.py中定义的内容，但不能使用colors和shapes的内容
> import drawing.colors：可使用colors的内容，但必须使用drawing.colors全限定名，当然__init__.py中的内容也可以使用 
> from drawing import shapes：可直接通过shapes.xxx来使用shapes模块中的内容，当然__init__.py中的内容也可以使用


### 探索模块

使用dir函数，可以获得模块中的所有属性，包括类、函数、变量等，另外如果模块中包含了__all__这样的变量，那么使用```from module import * ```的话只会导入__all__所包含的属性，其它的只能通过精确导入才行。

```python
# drawing/colors.py
def green():
    """
    just a demo function of green
    """
    pass
def red():pass

__all__ = ["green"]
```

```
>>> from drawing.colors import *
>>> green()
>>> red()
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
NameError: name 'red' is not defined
```

使用help函数可以获取对应函数的文档```help(green)```，如果想获取doc字段可以使用```print(green.__doc__)```。

通过模块的__file__属性可以获取这个模块的文件路径

```
>>> import drawing; print(drawing.__file__)
/Users/liqingshou/Work/xiaochai/batman/Python/drawing/__init__.py
```

### 一些标准库

sys：

> argv: 命令行参数，包括脚本名
> exit([arg]) : 退出当前程序，可通过可选参数指定返回值或错误消息
> modules:一个字典，将模块名映射到加载的模块
> path: 一个列表，包含要在其中查找模块的目录的名称
> platform : 一个平台标识符，如sunos5或win32
> stdin: 标准输入流——一个类似于文件的对象
> stdout: 标准输出流——一个类似于文件的对象
> stderr: 标准错误流——一个类似于文件的对象
       
os:

> environ:包含环境变量的映射
> system(command) 在子shell中执行操作系统命令
> sep 路径中使用的分隔符
> pathsep 分隔不同路径的分隔符
> linesep: 行分隔符('\n'、'\r'或'\r\n')
> urandom(n) :  返回n个字节的强加密随机数据

```
>>> os.environ
environ({'USER': 'liqingshou', 'COMMAND_MODE': 'unix2003', ...})
>>> os.system("pwd")
/Users/liqingshou/Work/xiaochai/batman/Python
0
>>> os.sep
'/'
>>> os.pathsep
':'
>>> os.linesep
'\n'
>>> os.urandom(10)
b'\x85\x9cs\xfa\xecA\xc5\xfa@\xa4'
```

fileinput:

> input([files[, inplace[, backup]]]) : 帮助迭代多个输入流中的行，如果指定了inplace为True，则原文件将被标准输出的内容所替代
> filename(): 返回当前文件的名称
> lineno(): 返回(累计的)当前行号
> filelineno(): 返回在当前文件中的行号
> isfirstline() : 检查当前行是否是文件中的第一行
> isstdin() : 检查最后一行是否来自sys.stdin
> nextfile() : 关闭当前文件并移到下一个文件
> close(): 关闭序列


```python
# 对于一系列输入了的文件，在开头添加文件名，在行末添加行号，只取前5行
import fileinput
for line in fileinput.input(inplace=False):
    if fileinput.isfirstline():
        print("# filename:", fileinput.filename(), 
        ", isstdin:", fileinput.isstdin())
    if fileinput.filelineno() >= 5: 
        fileinput.nextfile()
    line = line.rstrip()
    print('{:<50} # {:2d} # {} # '.format(
        line, fileinput.lineno(), 
        fileinput.filelineno()))
fileinput.close()

# 运行命令 python3 test.py test.py test.py
# 运行结果如下：
# filename: test.py , isstdin: False
# import fileinput                                   #  1 # 1 # 
# for line in fileinput.input(inplace=False):        #  2 # 2 # 
#     if fileinput.isfirstline():                    #  3 # 3 # 
#         print("# filename:", fileinput.filename(), #  4 # 4 # 
#         ", isstdin:", fileinput.isstdin())         #  5 # 5 # 
# # filename: test.py , isstdin: False
# import fileinput                                   #  6 # 1 # 
# for line in fileinput.input(inplace=False):        #  7 # 2 # 
#     if fileinput.isfirstline():                    #  8 # 3 # 
#         print("# filename:", fileinput.filename(), #  9 # 4 # 
#         ", isstdin:", fileinput.isstdin())         # 10 # 5 # 
```

集合：

新版本的Python中内置支持了set集合，创建方式使用花括号表示，注意空的花括号会被识别为字典。

集合上支持一些操作：

union: 取并集，与符号|一致
intersection： 取交集，与符号&一致
issubset: 调用者是否是参数指定集合的子集
differece: 调用集合排除掉参数集合后的结果，与符号-一致
symmetric_difference: 取两个集合不一样的用户，与^符号一致
add: 往集合中添加元素
remove: 从集合中删除元素

```
>>> type({})
<class 'dict'>
>>> type({1})
<class 'set'>
>>> {1,2,2,2,2,2,3}
{1, 2, 3}
>>> a = {1,2,3}
>>> b = {2,3,4}

>>> a | b
{1, 2, 3, 4}
>>> a.union(b)
{1, 2, 3, 4}

>>> a & b
{2, 3}
>>> a.intersection(b)
{2, 3}

>>> a.issubset(b)
False
>>> a.issubset(a)
True

>>> a.difference(b)
{1}
>>> a-b
{1}
>>> b-a
{4}

>>> a.symmetric_difference(b)
{1, 4}
>>> a ^ b
{1, 4}

>>> a.add(5)
>>> a
{1, 2, 3, 5}
```




集合是可变的，所以不能用作字典的键；另外集合的成员只能是不可变的值，所以现实中的集合的集合需要借助于frozenset类型（不可变集合）来实现：


```
>>> a = {1}
>>> b = {2}
>>> a.add(b)
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: unhashable type: 'set'
>>> a.add(frozenset(b))
>>> a
{1, frozenset({2})}
```


堆：

Python中的堆数据结构使用列表与一些函数配合实现，主要提供的函数有以下这些：

> heappush(heap, x)  : 将x压入堆中
> heappop(heap)  :  从堆中弹出最小的元素
> heapify(heap)  : 让列表具备堆特征
> heapreplace(heap, x)  : 弹出最小的元素，并将x压入堆中
> nlargest(n, iter)  : 返回iter中n个最大的元素
> nsmallest(n, iter) : 返回iter中n个最小的元素

```
>>> l = [99,2,87,27,19,50,92,43,39]
>>> from heapq import *
>>> l
[99, 2, 87, 27, 19, 50, 92, 43, 39]
>>> heapify(l)
>>> l
[2, 19, 50, 27, 99, 87, 92, 43, 39]
>>> heappop(l)
2
>>> l
[19, 27, 50, 39, 99, 87, 92, 43]
>>> heappop(l)
19
>>> heapreplace(l, 1)
27
>>> l
[1, 39, 50, 43, 99, 87, 92]
>>> nlargest(2, l)
[99, 92]
>>> l
[1, 39, 50, 43, 99, 87, 92]
>>> nsmalle
```

双端队列：

双端队列在collections模块中的deque类型，以下示例展示了常用的函数使用方式：

```
>>> from collections import deque
>>> q = deque([1,2,3,4,5])
>>> q
deque([1, 2, 3, 4, 5])
>>> q.append(6)
>>> q.appendleft(0)
>>> q
deque([0, 1, 2, 3, 4, 5, 6])
>>> q.pop()
6
>>> q.popleft()
0
>>> q
deque([1, 2, 3, 4, 5])
>>> q.rotate(3)
>>> q
deque([3, 4, 5, 1, 2])
>>> q.rotate(-1)
>>> q
deque([4, 5, 1, 2, 3])
```

time:

> asctime([tuple]) : 将时间元组转换为字符串
> localtime([secs]) : 将秒数转换为表示当地时间的日期元组
> mktime(tuple) : 将时间元组转换为当地时间
> sleep(secs) : 休眠(什么都不做)secs秒
> strptime(string[, format]) : 将字符串转换为时间元组
> time(): 当前时间(从新纪元开始后的秒数，以UTC为准)
   
以上的tuple为8元组，分别为(年,月,日,时,分,秒,星期,儒略日,夏令时)

```
>>> l = time.localtime()
>>> l
time.struct_time(tm_year=2020, tm_mon=6, tm_mday=20, tm_hour=20, tm_min=12, tm_sec=52, tm_wday=5, tm_yday=172, tm_isdst=0)
>>> l = list(l)
>>> l[0] = 2019
>>> time.asctime(tuple(l))
'Sat Jun 20 20:12:52 2019'
>>> time.time()
1592655205.469694
```

random：

> random() : 返回一个0~1(含)的随机实数
> getrandbits(n) :  以长整数方式返回n个随机的二进制位
> uniform(a, b) : 返回一个a~b(含)的随机实数
> randrange([start], stop, [step])  : 从range(start, stop, step)中随机地选择一个数
> choice(seq) : 从序列seq中随机地选择一个元素
> shuffle(seq[, random])  : 就地打乱序列seq
> sample(seq, n) : 从序列seq中随机地选择n个值不同的元素
  
```
>>> import random
>>> random.random()
0.2918300245721943
>>> random.getrandbits(10)
831
>>> random.uniform(20,30)
21.144689897763417
>>> random.randrange(1000, 2000, 1)
1890
>>> random.choice(range(100))
49
>>> l = list(range(10))
>>> random.shuffle(l)
>>> l
[9, 8, 2, 4, 1, 5, 3, 7, 0, 6]
>>> random.sample(l, 3)
[8, 4, 6] 
```
   

shelve:

shelve算是一个将python的数据结构序列化到文件中的一个方便的类

```bash
➜  Python git:(master) ✗ python3
Python 3.7.7 (default, Mar 10 2020, 15:43:33) 
>>> import shelve
>>> s = shelve.open('test.dat')
>>> s['x'] = ['a','b','c']
>>> s['y'] = {"a" : 1}
>>> 
➜  Python git:(master) ✗ python3
Python 3.7.7 (default, Mar 10 2020, 15:43:33) 
>>> import shelve
>>> s = shelve.open('test.dat')
>>> s['x']
['a', 'b', 'c']
>>> s['y']
{'a': 1}
```


```python
import shelve

def enter_command():
    cmd = input("Enter command(? for help):")
    cmd = cmd.strip().lower()
    return cmd

def store_person(db):
    id = input("Enter ID:")
    person = {}
    person['name'] = input('Enter name:')
    person['age'] = input('Enter age:')
    db[id] = person

def lookup_person(db):
    id = input("Enter ID:")
    print(db[id])

def print_help():
    print("""The available commands are:
    store: stores information
    lookup: looks up a person by ID
    quit: save change and exit
    ?: prints this message
    """)

def main():
    database = shelve.open("./database.dat")
    try:
        while True:
            cmd = enter_command()
            if cmd == 'store':
                store_person(database)
            elif cmd == 'lookup':
                lookup_person(database)
            elif cmd == '?':
                print_help()
            elif cmd == 'quit':
                return 
    finally:
        database.close()

if __name__ == '__main__': main()


# 以下是运行结果

# ➜  Python git:(master) ✗ python3 database.py
# Enter command(? for help):'?
# The available commands are:
#     store: stores information
#     lookup: looks up a person by ID
#     quit: save change and exit
#     ?: prints this message
#     
# Enter command(? for help):'store
# Enter ID:1
# Enter name:Lee
# Enter age:10
# Enter command(? for help):'store
# Enter ID:2
# Enter name:Chen
# Enter age:12
# Enter command(? for help):'quit
# ➜  Python git:(master) ✗ python3 database.py
# Enter command(? for help):lookup
# Enter ID:1
# {'name': 'Lee', 'age': '10'}
# Enter command(? for help):lookup
# Enter ID:2
# {'name': 'Chen', 'age': '12'}
# Enter command(? for help):quit
```

re正则

compile(pattern[, flags])  ： 根据包含正则表达式的字符串创建模式对象 
search(pattern, string[, flags])：  在字符串中查找模式
match(pattern, string[, flags]) ：在字符串开头匹配模式，注意与search的不同，match是整个字符串匹配，而search可以部分匹配
split(pattern, string[, maxsplit=0]) : 根据模式来分割字符串
findall(pattern, string):  返回一个列表，其中包含字符串中所有与模式匹配的子串
sub(pat, repl, string[, count=0]) :  将字符串中与模式pat匹配的子串都替换为repl 
escape(string) : 对字符串中所有的正则表达式特殊字符都进行转义


```
>>> p = re.compile('[0-9]+')
>>> p
re.compile('[0-9]+')
>>> re.search(p, "ik2131,k21lsdf")
<re.Match object; span=(2, 6), match='2131'>
>>> re.match(p, "ik2131,k21lsdf")
>>> re.match(p, "32423423")
<re.Match object; span=(0, 8), match='32423423'>
>>> re.search('[a-z]+', '4323kljjk32jlj342')
<re.Match object; span=(4, 9), match='kljjk'>
>>> p.split("3423kjlkjl2432kjl34j24l34")
['', 'kjlkjl', 'kjl', 'j', 'l', '']
>>> re.findall('[a-z]+', '4323kljjk32jlj342')
['kljjk', 'jlj']
>>> p.sub("....", '4323kljjk32jlj342')
'....kljjk....jlj....'
>>> re.escape("[0-9]+")
'\\[0\\-9\\]\\+'
>>> re.sub("([0-9]+?)", r"\1\1", "kjl23423,32424")
'kjl2233442233,3322442244'
>>> re.sub("([0-9]+)", r"\1\1", "kjl23423,32424")
'kjl2342323423,3242432424'
```

match和search如果没有匹配返回 None，匹配上时会返回re.Match对象，此对象上可以应用如下方法：

groups() : 以元组的方式返回所有的匹配编组
group([group1, ...])  : 获取与给定子模式(编组)匹配的子串
start([group]) ：返回与给定编组匹配的子串的起始位置 
end([group]) ： 返回与给定编组匹配的子串的终止位置(与切片一样，不包含终止位置)
span([group]) ：返回与给定编组匹配的子串的起始和终止位置

```
>>> str = "Info, Name:Lee,Age:23,From:China"
>>> g = re.match(".*Name:([a-zA-Z.]+),[ ]?Age:([0-9]+).*", str)
>>> g.groups()
('Lee', '23')
>>> g.group(2)
'23'
>>> str[g.start(1):g.end(1)]
'Lee'
>>> g.span(1)
(11, 14)
```
  

其它模块：

argparse: 解析命令行参数，比sys.argv更加好用

cmd: 这个模块让你能够编写类似于Python交互式解释器的命令行解释器。你可定义命令， 让用户能够在提示符下执行它们

csv: csv文件的解析与写入

json: json字符串的处理

datetime:  更多的日期操作支持

difflib: 比较两个序列的相似程度

enum:对于枚举的支持

functools: 

hashlib:

itertools:

logging:

statistics:

timeit、profile、trace：

## 文件

open函数用于打开文件，可以指定一个模式选项：

r:读取模式(默认值)
w:写入模式
x：独占写入模式
a:追加加模式 
b:二进制模式(与其他模式结合使用)
t:文本模式(默认值，与其他模式结合使用)
+: 读写模式(与其他模式结合使用)，r+和w+都表示读写，但是w+会截断文件

open返回的对象，可以使用write，read，close这些函数；sys.stdin也是一个资源对象：

```
>>> f = open("./a", "w+")
>>> f.write("helloworld\n")
11
>>> f.writelines(["1\n", "2\n"])
>>> f.close()
>>> f = open("./a", "r+")
>>> f.read(1)
'h'
>>> f.readline()
'elloworld\n'
>>> f.seek(0)
0
>>> f.readlines()
['helloworld\n', '1\n', '2\n']
>>> f.close()
```
  
文件打开后需要调用close()关闭文件，为了防止文件被锁定无法修改，或者占用太多的文件描述符号。一般我们使用以下语句来处理关闭文件：

```python
f = open("./a", "w+")
try:
    # 写入数据
    f.write("xxx\n")
finally:
    f.close()

# with语句专门用于处理此情况，称之为上下文管理器

with open("./a", "w+") as f:
    f.write("xxx\n")
```

在Python中，打开的文件是可以直接迭代的，相当于每一次读取一行的文件内容：```with open("./clz.py", "r") as f: list(f)```


## GUI

通过简单的文件加载编辑器来演示tkinter的使用：

```python
from tkinter import *
from tkinter.scrolledtext import ScrolledText

def load():
    with open(filename.get()) as f:
        # 表示删除第一行第0个字符开始，到结束
        contents.delete('1.0', END)
        contents.insert(INSERT, f.read())
def save():
    with open(filename.get(), 'w') as f:
        f.write(contents.get('1.0', END))

top = Tk()
top.title("Editor")

contents = ScrolledText()
contents["bg"] = "black"
contents["fg"] = "white"
contents.pack(side=BOTTOM, expand=True, fill=BOTH)

filename = Entry()
filename.pack(side=LEFT, expand=True, fill=X)

openBtn = Button(text="Open", command=load, highlightbackground='black')
openBtn.pack(side=LEFT)
saveBtn = Button(text="Save", command=save, highlightbackground='black')
saveBtn.pack(side=LEFT)
mainloop()

```

## 数据库

Python中对数据库定义了一些标准，所有的第三方数据库需要按照这个标准来实现：

### 全局变量

apilevel: 用于指代这个模块所支持的标准数据库接口的版本号，Python中的DB API2.0指出这个值可以是1.0或者2.0，如果不存在这个变量，说明这个模块不兼容DB API2.0标准。

threadsafety: 模块的线程安全级别，3表示绝对的线程安全，2表示线程可以共享模块和连接但不能共享游标，1表示可共享模块，但不能共享连接，0表示也不能共享模块。

paramstyle: 在SQL查询中使用的参数风格支持，format表示使用标准的字符串格式，如%s等；pyformat使用字典的形式如%(foo)s；qmark使用问号占位，numeric使用数字编号占位如:1、:2这样；named使用命名占位如:foobar。

### 异常

DB API定义的一些通用异常：

StandardError： 所有异常的超类
Warning：发生非致命问题时引发
Error：所有错误条件的超类
InterfaceError：与接口(而不是数据库)相关的错误
DatabaseError：与数据库相关的错误的超类
DataError：与数据相关的问题，如值不在合法的范围内
OperationalError ：数据库操作内部的错误
IntegrityError ：关系完整性遭到破坏，如键未通过检查
InternalError ：数据库内部的错误，如游标无效
ProgrammingError ：用户编程错误，如未找到数据库表
NotSupportedError：请求不支持的功能，如回滚


### 一些标准函数

connect: 连接数据函数，其它参数支持有dsn(数据源名称，因数据库而异)；user（用户名）；password（密码）；host（主机）；database（数据库名称）。connect会返回连接对象，表示当前的数据库会话，支持以下方法：

close(): 关闭连接，之后连接对象以及其游标都不可用；注意对于支持事务的数据库，关闭连接会把没有提交的事务回滚了，所以注意在关闭之前提交事务

commit(): 提交事务，如果此数据库不支持事务，则这个函数啥也不做

rollback(): 回滚操作，如果数据库不支持事务，则抛出异常

cursor(): 返回连接的游标对象，使用游标来执行SQL查询和查看结果

游标对象支持下以方法和属性：

callproc(name[, params]) ：使用指定的参数调用指定的数据库过程(可选)
close()：关闭游标。关闭后游标不可用
execute(oper[, params])  ：执行一个SQL操作——可能指定参数
executemany(oper, pseq) ：执行指定的SQL操作多次，每次都序列中的一组参数
fetchone() ：以序列的方式取回查询结果中的下一行;如果没有更多的行，就返回None
fetchmany([size]) ：取回查询结果中的多行，其中参数size的值默认为arraysize
fetchall()：以序列的序列的方式取回余下的所有行
nextset() ：跳到下一个结果集，这个方法是可选的
setinputsizes(sizes) ：用于为参数预定义内存区域
setoutputsize(size[, col])：为取回大量数据而设置缓冲区长度

description ：由结果列描述组成的序列(只读)
rowcount ：结果包含的行数(只读)
arraysize：fetchmany返回的行数，默认为1

### 类型

Date(year, month, day) ：创建包含日期值的对象
Time(hour, minute, second) ：创建包含时间值的对象
Timestamp(y, mon, d, h, min, s) ：创建包含时间戳的对象
DateFromTicks(ticks) ：根据从新纪元开始过去的秒数创建包含日期值的对象
TimeFromTicks(ticks) 根据从新纪元开始过去的秒数创建包含时间值的对象
imestampFromTicks(ticks) ：根据从新纪元开始过去的秒数创建包含时间戳的对象
Binary(string)：创建包含二进制字符串值的对象
STRING：描述基于字符串的列(如CHAR)
BINARY：描述二进制列(如LONG或RAW)
NUMBER：描述数字列
DATETIME：描述日期/时间列
ROWID:描述行ID列

### 一些例子

使用sqlite存储和查询数据

```python
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
```
      
  
如果是mysql的话，需要安装pymysql扩展```pip3 install cryptography pymysql```；另外可以使用dokcer启动一个mysql:

```bash
$ docker run -p 3306:3306 --name mysql -e MYSQL_ROOT_PASSWORD=123456 -d mysql
$ docker exec -it mysql bash
root@088053eba10b:/# mysql -uroot -p123456
mysql> create database test;
```

```python
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
```
  
## 网络

使用socket展示一个简单的客户端和服务端：

```python
import socket
import sys
def server():
    s = socket.socket()
    s.bind(("127.0.0.1", 8888))
    s.listen(5) # 这个参数是backlog
    while True:
        c, addr = s.accept()
        # Got connection from  ('127.0.0.1', 64504)
        print('Got connection from ', addr)
        c.sendall(b'Thank you for connecting')
        c.close()

def client():
    s = socket.socket()
    s.connect(("127.0.0.1", 8888))
    print(s.recv(1024))

# 一个简单地只能每一次处理一个请求的server和client示例
if len(sys.argv) >= 2 and sys.argv[1] == "server":
    server()
else:
    client()
```

使用urllib和urllib2这两个网络库可以很方便地访问网络文件：


urlopen返回的对象是类似于文件资源的对象，所以可以使用类似read等方法以及迭代等，读取出来的内容可以通过正则来获取想要的数据。

urlretrieve可以下载文件到指定路径，如果文件没有指定，会存储在一个临时的位置，可以调用urlcleanup函数（不带参数）来直接清理对应的文件。




```
>>> import urllib.request as urlreq
>>> webpage = urlreq.urlopen('http://baidu.com')
>>> webpage.read()
b'<html>\n<meta http-equiv="refresh" content="0;url=http://www.baidu.com/">\n</html>\n'
>>> urlreq.urlretrieve("http://baidu.com", "./a")
('./a', <http.client.HTTPMessage object at 0x1057ce730>)

>>> import urllib.parse
>>> urllib.parse.urlencode({"callback":"http://baidu.com","name":"Lee"})
'callback=http%3A%2F%2Fbaidu.com&name=Lee'
```


### SocketServer

之前的server/client的示例，同时只能处理一个请求，使用SocketServer这一模块，可以有多种方式同时处理多个请求：

先来看一个简单的server端：

```python
# 使用socketserver来创建服务端
from socketserver import TCPServer, StreamRequestHandler
def sServer():
    class Handler(StreamRequestHandler):
        def handle(self):
            addr = self.request.getpeername()
            print('Got connection from', addr)
            self.wfile.write(b'Thank you for connecting')
    server = TCPServer(('127.0.0.1', 8888), Handler)
    server.serve_forever()
```

使用forking和创建线程来同时处理多个请求的代码实现很简单：
```python
# 通过每来一个请求就fork进程的方式来同时处理多个请求
# 在client请求时，可以使用ps -ef| grep python| grep server来查看新起的进程
def forkingServer():
    class Server(ForkingMixIn, TCPServer): pass
    server = Server(('127.0.0.1', 8888), Handler)
    server.serve_forever()

# 通过每来一个请求创建一个线程的方式来同时处理多个请求
def threadServer():
    class Server(ThreadingMixIn, TCPServer): pass
    server = Server(('127.0.0.1', 8888), Handler)
    server.serve_forever()
```

使用多路复用的方式来实现同时处理多个请求：

```python
import select
# 这个例子中使用select来同时保持多个连接，并且可以随时响应任意连接的数据写入
def selectServer():
    s = socket.socket()
    s.bind(("127.0.0.1", 8888))
    s.listen(5) # 这个参数是backlog
    inputs = [s]
    while True:
        rs, ws, es = select.select(inputs, [], [])
        for r in rs:
            if r is s:
                c, addr = s.accept()
                print('Got connection from ', addr)
                inputs.append(c)
            else:
                try:
                    data = r.recv(1024)
                    disconnected = not data
                except socket.error:
                    disconnected = True
                if disconnected:
                    print(r.getpeername(), "disconnected")
                    inputs.remove(r)
                else:
                    print("from ", r.getpeername(), ":", data)
```

需要对应的给client端进行一些改造，使得能够响应输入并发送这些数据：

```python
def iclient():
    s = socket.socket()
    s.connect(("127.0.0.1", 8888))
    while True:
        i = input("enter:")
        s.sendall( bytes(i, "utf8"))
```

使用poll和epoll(mac中没有epoll只有kqueue)的例子就不在这里处理了。


使用twisted基于事件的网络库```pip3 install twisted```


```python
from twisted.internet import reactor
from twisted.internet.protocol import Protocol, Factory 

def twistedServer():
    class SimpleLogger(Protocol):
        def connectionMade(self):
            print('Got connection from', self.transport.client)
        def connectionLost(self, reason): 
            print(self.transport.client, 'disconnected')
        def dataReceived(self, data): 
            print("from ", self.transport.client, ":", data)
    factory = Factory()
    factory.protocol = SimpleLogger
    reactor.listenTCP(8888, factory)
    reactor.run()
```

## Web

在网页抓取工作中，由于HTML编写得并不都十分规范，所以就需要使用像[Tidy](https://github.com/htacg/tidy-html5)之类的工具将其修复。

例如如下这个不规范的html文件，使用```tidy -o fixed.html bad.html```命令转换完成之后的效果如下面的代码:

```html
<h1>Pet Shop <h2>Complaints</h3>
<p>There is <b>no <i>way</b> at all</i> we can accept returned parrots.
<h1><i>Dead Pets</h1>
<p>Our pets may tend to rest at times, but rarely die within the
    warranty period.
<i><h2>News</h2></i>
<p>We have just received <b>a really niparrot.
<p>It's really nice.</b>
<h3><hr>The Norwegian Blue</h3>
<h4>Plumage and <hr>pining behavior</h4>
<a href="#norwegian-blue">More information<a>
<p>Features:
<body>
<li>Beautiful plumage
```

```html
<!DOCTYPE html>
<html>
<head>
<meta name="generator" content=
"HTML Tidy for HTML5 for Apple macOS version 5.6.0">
<title></title>
</head>
<body>
<h1>Pet Shop</h1>
<h2>Complaints</h2>
<p>There is <b>no <i>way</i></b> <i>at all</i> we can accept
returned parrots.</p>
<h1><i>Dead Pets</i></h1>
<p><i>Our pets may tend to rest at times, but rarely die within the
warranty period. </i></p>
<h2><i>News</i></h2>
<p>We have just received <b>a really niparrot.</b></p>
<p><b>It's really nice.</b></p>
<hr>
<h3>The Norwegian Blue</h3>
<h4>Plumage and</h4>
<hr>
<h4>pining behavior</h4>
<a href="#norwegian-blue">More information</a>
<p>Features:</p>
<li>Beautiful plumage</li>
</body>
</html>
```

也可以使用subprocess模块来通过Python来调用tidy命令，也可以使用pytidylib这个封装器来使用。


有了标准的html之后，可以使用HTMLParser来进行内容的解析了，它是基于事件的方式来处理，需要继承HTMLParser类，并实现对应的回调函数：

```python
from html.parser import HTMLParser

class GrabH1(HTMLParser):
    h1 = False
    def handle_starttag(self, tag, attrs):
        if tag == "h1":
            self.h1 = True
    def handle_data(self, data):
        if self.h1 == True:
            print("DATA:", data)

    def handle_endtag(self, tag):
        if self.h1 and tag == "h1":
            self.h1 = False
text = open("./fixed.html").read()
parser = GrabH1()
parser.feed(text)
parser.close()

# DATA: Pet Shop
# DATA: Dead Pets
```

[Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/)是一个可以从HTML或XML文件中提取数据的Python库。安装：``` pip install beautifulsoup4```，[文档参考](https://beautifulsoup.readthedocs.io/zh_CN/v4.4.0/#)

```python
# beautiful soup试用
from urllib.request import urlopen 
from bs4 import BeautifulSoup

text = urlopen('https://www.crummy.com/software/BeautifulSoup/').read()

soup = BeautifulSoup(text, 'html.parser')
links = {}
for a in soup.find_all('a'):
    try:
        links[a.string] = a["href"]
    except KeyError: pass

print(links)

# {'Download': '#Download', 'Documentation': 'bs4/doc/', 'Hall of Fame': '#HallOfFame', 'For enterprise': 'enterprise.html', 'Source': 'https://code.launchpad.net/beautifulsoup',...}
```

书中说的使用cgi来架设web服务器在目前看来已经淘汰了，我们使用[uwsgi](https://uwsgi-docs.readthedocs.io/en/latest/)和fastcgi来搭建服务器吧。

```
$ pip3 install uwsgi
$ brew install uwsgi
```

uwsgi提供了直接作为web server的能力，假设我们有文件uwsgi.py如下，可以直接运行```uwsgi --http :9090 --wsgi-file uwsgi.py ```，就可以在浏览器上访问http://localhost:9090/ 获得Hello World的输出内容：

```python
# uwsgi在启动时就会运行指定的python文件，并在每一次请求的时候查找application函数，这可以通过配置来修改
def application(env, start_response):
    start_response('200 OK', [('Content-Type','text/html')])
    return [b"Hello World"]
```

也可以与nginx配合，使用uwsgi协议来进行通信：

```
$ uwsgi --socket 127.0.0.1:3031 --wsgi-file uwsgi.py
```

nginx的配置如下：

```conf
# 配置文件地址为：/usr/local/etc/nginx/nginx.conf
# 启动命令 brew services start nginx
location / {
    include uwsgi_params;
    uwsgi_pass 127.0.0.1:3031;
}
```

这样在浏览器中访问http://localhost:8080 即可访问与之前一致的页面（nginx监听的是8080端口）。



Python的Web框架[Flask](http://docs.jinkan.org/docs/flask/)：

创建文件myflaskapp.py:

```python
from flask import Flask, request

app = Flask(__name__)
@app.route('/')
def index():
    return "<span style='color:red'>I am app 1</span>"

@app.route('/page', methods=['POST', 'GET'])
def page():
    return "GET:{}\nMETHOD:{}\nPOST:{}\n".format(request.args, request.method, request.form)

@app.route('/hello/<name>')
def hello(name=None):
    return render_template('hello.html', name=name)
```

flask支持[Jinja2](http://jinja.pocoo.org/)模板引擎，并会在templates目录下找render_template指定的文件，以下是templates/hello.html

```html
<!doctype html>
<title>Hello from Flask</title>
{% if name %}
  <h1>Hello {{ name }}!</h1>
{% else %}
  <h1>Hello World!</h1>
{% endif %}
```

运行uwsgi程序```uwsgi --socket 127.0.0.1:3001 --wsgi-file myflaskapp.py --callable=app```，这个app就是上面代码中app = Flask(__name__)创建的入口函数，如果这个启动比较麻烦，可以使用配置文件uwsgi.ini，启动时```uwsgi uwsgi.ini```：

```ini
[uwsgi]
socket=:3001
wsgi-file=./myflaskapp.py
callable=app
```


```
$ curl 'http://localhost:8080/page?abcdefg=2xxx&dkfj=sfa' -d 'a=1&b=1'
GET:ImmutableMultiDict([('abcdefg', '2xxx'), ('dkfj', 'sfa')])
METHOD:POST
POST:ImmutableMultiDict([('a', '1'), ('b', '1')])
```

还有一些其它的web框架：[Django](https://www.djangoproject.com/)、[TurboGears](https://www.turbogears.org/)、[web2py](http://web2py.com/)、[Grok](https://pypi.python.org/pypi/grok)、[Zope2](https://pypi.python.org/pypi/Zope2)、[Pyramid](https://trypyramid.com/)。


## 测试

介绍两个测试库doctest和unittest，这两测试库Python都自带，以及代码检查工具pylint和pychecker

### doctest

从函数的文档中获取交互式命令相关的内容做为测试case：

```python
def square(x): 
    '''
    计算平方并返回结果
    >>> square(2) 
    4
    >>> square(3) 
    9
    '''
    return x * x

if __name__ == '__main__':
    import doctest
    # doctest将测试本文件内的函数，用例是从函数的注释中获取
    doctest.testmod()

# $ python3 doct.py -v
# Trying:
#     square(2) 
# Expecting:
#     4
# ok
# Trying:
#     square(3) 
# Expecting:
#     9
# ok
# 1 items had no tests:
#     __main__
# 1 items passed all tests:
#    2 tests in __main__.square
# 2 tests in 2 items.
# 2 passed and 0 failed.
# Test passed.
```

### unittest

使用unittest来测试刚才的square函数，我们可以把测试用例使用单独的文件来写。unittest.main()方法运行所有的测试，包括所有TestCase的子类以及类中的test打头的方法。另外它还提供了setUp和tearDown等在测试不同阶段可以回调的函数，方便做数据的准备了清理。


```python
from doct import square
import unittest

class ProductTestCase(unittest.TestCase):
    def test_integers(self):
        for x in range(-10, 10):
            self.assertEqual(square(x), x*x, 'Integer psquare failed')
    def test_floats(self):
        for x in range(-10, 10):
            self.assertEqual(square(x/10), x/10*(x/10), 'Float psquare failed')

if __name__ == "__main__":
    unittest.main()

# $ python3 unitt.py -v
# test_floats (__main__.ProductTestCase) ... ok
# test_integers (__main__.ProductTestCase) ... ok
# 
# ---------------------------------------------------------------------
# Ran 2 tests in 0.000s
```


### pylint和pychecker

pylint将给出一些代码的建议，如是否换行，缺少空格等等：

```
$ brew install pylint
$ pylint doct.py 
************* Module doct
doct.py:1:14: C0303: Trailing whitespace (trailing-whitespace)
doct.py:4:17: C0303: Trailing whitespace (trailing-whitespace)
doct.py:6:17: C0303: Trailing whitespace (trailing-whitespace)
doct.py:18:15: C0303: Trailing whitespace (trailing-whitespace)
doct.py:23:15: C0303: Trailing whitespace (trailing-whitespace)
doct.py:39:15: C0303: Trailing whitespace (trailing-whitespace)
doct.py:47:0: C0304: Final newline missing (missing-final-newline)
doct.py:1:0: C0114: Missing module docstring (missing-module-docstring)
doct.py:1:0: C0103: Argument name "x" doesn't conform to snake_case naming style (invalid-name)

--------------------------------------------------------------------
Your code has been rated at -8.00/10 (previous run: -8.00/10, +0.00)
```

### 性能分析

标准库中的profile用于性能分析，他可以给出每一个函数的调用次数以及执行的时间，还可以将结果保存于文件中，使用pstats这个模块来分析。profile模块也有对应更快件的C语言版本cProfile

## 扩展Python

使用Jython来直接扩展：


标准版Python:CPython的扩展：

编写扩展思路与PHP写扩展的思路是一样的，只是Python中有一个swig工具可以简化这一部分的工作。

例如我们要写一个简单的回文检测函数，涉及到palindrome.c和palindrome.i两个文件，代码如下：

```c
#include <string.h>
int is_palindrome(char *text) { 
    int i, n=strlen(text);
    for (i = 0; i <= n/2; ++i) {
        if (text[i] != text[n-i-1]) 
            return 0; 
    }
    return 1; 
}
```

```c
%module palindrome
%{
#include <string.h> 
%}
extern int is_palindrome(char *text);
```

使用
```bash
swig -python -module myp palindrome.i 
```
命令会自动生成palindrome_wrap.c文件，并指定扩展的模块名称为myp，生成myp.py这个模块的封装代码；

然后将这两个文件.c文件编译成共享链接库：

```bash
gcc -I/usr/local/Cellar/python@3.8/3.8.3/Frameworks/Python.framework/Versions/3.8/include/python3.8/ -shared palindrome_wrap.c palindrome.c -lpython3.8 -L/usr/local/Cellar/python@3.8/3.8.3/Frameworks/Python.framework/Versions/3.8/lib/ -o _myp.so
```

其中-I指定头文件的搜索目录，-L指定了链接库的搜索目录，而-l指定了需要链接的python的动态链接库，这是我在mac上的路径，其它环境有可能不一样；注意动态链接库的名称需要是模块名前添加下划线。

这样就可以使用myp这个模块了：

```bash
$ PYTHONPATH=$PYTHOPATH:./ python3
>>> import myp
m>>> myp.is_palindrome("abba")
1
>>> import _myp
>>> _myp.is_palindrome("abbaa")
0
```
可以看出如果使用import myp则加载的是myp.py，而这个封装文件最终也是使用的_myp.so这个动态库，与直接使用import _myp是一样的结果。


如果手写扩展需要处理好引用计数相关的问题，可以参考[此](https://www.cnblogs.com/jianmu/p/7367698.html)与[此](https://docs.python.org/2.0/ext/intro.html)。以下是不用swig手写的回文字符串检测palindrome1.c：

```c
#include <Python.h>
#include <string.h>

/**
 * 定义的目标函数，函数的返回值必须是*PyObject，self指向模块本身，args参数包含所有传入的参数
 */
static PyObject *is_palindrom(PyObject *self, PyObject *args){
    int i, n;
    const char *text;
    // 传入的参数可以使用PyArg_ParseTuple按一定的格式解析到对应的变量中，其中s表示是字符串
    if (!PyArg_ParseTuple(args, "s", &text)){
        return NULL;
    }
    n = strlen(text);
    // 这次将返回值改成返回True和False
    for (i = 0; i<= n/2; i++){
        if (text[i] != text[n-i-1]){
            Py_INCREF(Py_False);
            return Py_False;
        }
    }
    // 需要增加Py_True的引用计数的数量
    Py_INCREF(Py_True);
    // 如果是要返回整数1的话可以使用Py_BuildValue("i", 1);
    return Py_True;
}

// 导出的方法列表定义，在PyModuleDef中使用，而PyModuleDef又在PyInit_modulename上使用
static PyMethodDef PalindromeMethods[] = {
    // 名称(这影响在python中调用时的名字)、具体的函数、参数类型、文档
    {"is_palindrom1", is_palindrom, METH_VARARGS, "Dected palindromes"},
    // 列表结束标志
    {NULL, NULL, 0, NULL}
};

static struct PyModuleDef palindrome = 
{
    PyModuleDef_HEAD_INIT,
    "palindrome1", // 模块名，好像改完之后没有啥效果，只是在help时会在NAME里体现
    "", // 文档
    -1, // 存储在全局变量中的信号状态
    PalindromeMethods // 方法列表
};

// 初始化模块的函数，PyInit_modulename，名字必须是这样的命名规则
PyMODINIT_FUNC PyInit_palindrome1(void){
    return PyModule_Create(&palindrome);
}


// 编译: gcc -I/usr/local/Cellar/python@3.8/3.8.3/Frameworks/Python.framework/Versions/3.8/include/python3.8/ -shared  -lpython3.8 -L/usr/local/Cellar/python@3.8/3.8.3/Frameworks/Python.framework/Versions/3.8/lib/ palindrome1.c -o palindrome1.so -v

// >>> import palindrome1
// >>> palindrome1.is_palindrom1("FD")
// False
// >>> palindrome1.is_palindrom1("ABBA")
// True
```

另外一种不用手工编译的方式就是使用setuptools提供的方法：

```python
from setuptools import setup, Extension

setup(
    name = 'palindrome1',
    version = '1.0',
    ext_modules = [
        Extension('palindrome1', ['palindrome1.c'])
    ]
)
```

```
$ python3 setup.py build_ext
running build_ext
building 'palindrome1' extension
creating build
creating build/temp.macosx-10.15-x86_64-3.8
clang -Wno-unused-result -Wsign-compare -Wunreachable-code -fno-common -dynamic -DNDEBUG -g -fwrapv -O3 -Wall -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX10.15.sdk -I/Library/Developer/CommandLineTools/SDKs/MacOSX10.15.sdk/usr/include -I/Library/Developer/CommandLineTools/SDKs/MacOSX10.15.sdk/System/Library/Frameworks/Tk.framework/Versions/8.5/Headers -I/usr/local/include -I/usr/local/opt/openssl@1.1/include -I/usr/local/opt/sqlite/include -I/usr/local/Cellar/python@3.8/3.8.3/Frameworks/Python.framework/Versions/3.8/include/python3.8 -c palindrome1.c -o build/temp.macosx-10.15-x86_64-3.8/palindrome1.o
creating build/lib.macosx-10.15-x86_64-3.8
clang -bundle -undefined dynamic_lookup -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX10.15.sdk build/temp.macosx-10.15-x86_64-3.8/palindrome1.o -L/usr/local/lib -L/usr/local/opt/openssl@1.1/lib -L/usr/local/opt/sqlite/lib -o build/lib.macosx-10.15-x86_64-3.8/palindrome1.cpython-38-darwin.so
$ cd build/lib.macosx-10.15-x86_64-3.8
$ PYTHONPATH=$PYTHOPATH:./ python3
>>> import palindrome1
>>> palindrome1.is_palindrom1("daf")
False
>>> palindrome1.__file__
'/Users/liqingshou/Work/xiaochai/batman/Python/ext/build/lib.macosx-10.15-x86_64-3.8/palindrome1.cpython-38-darwin.so'
>>>
```


## 程序打包

程序打包的概念与PHP的phar文件一样，在Python中有两种格式egg和wheel(后缀为whl)，wheel将会逐渐取代egg。另外pi2exe扩展可以打包成windows平台的exe文件，而且不需要用户安装额外的解释器。

通过在PyPI上注册还可以让别人使用pip来安装你开发的包，这块可以参考[官方教程](https://packaging.python.org/tutorials/packaging-projects/)。

```python
# setuptools可以帮忙处理一些事情
# 运行python3 setup.py install将在dist目录下生成egg文件
# setup.py文件内容
from setuptools import setup

setup(
    name = "hello",
    version = "1.0",
    description = "simple example",
    author = "Lee",
    py_modules = ["hello"]
)
```

## 参考

[Python的MRO](https://blog.csdn.net/come_from_pluto/article/details/90483447)

[Python部署web开发程序的几种方法](https://www.cnblogs.com/softidea/p/6855214.html)

[CGI、FastCGI、WSGI、uwsgi、uWSGI](https://blog.csdn.net/qq_32767041/article/details/81227348#4-uwsgi)
