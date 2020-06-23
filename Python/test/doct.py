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

# 以下是出错的情况：
# *********************************************************************
# File "doct.py", line 6, in __main__.square
# Failed example:
#     square(3) 
# Expected:
#     9
# Got:
#     6
# *********************************************************************
# 1 items had failures:
#    1 of   2 in __main__.square
# ***Test Failed*** 1 failures.