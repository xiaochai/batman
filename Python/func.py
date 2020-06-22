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