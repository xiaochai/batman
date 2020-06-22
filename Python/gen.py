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