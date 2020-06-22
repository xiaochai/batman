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
