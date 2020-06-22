# 异常的基本用法
try:
    x = int(input('Enter the first number: '))
    y = int(input('Enter the second number: ')) 
    print(x / y)
except ZeroDivisionError: # 如果y为0，则为ZeroDivisionError，走此流程
    print("The second number can't be zero!")
except Exception as e: # 其它的Exception，走此流程，并捕获了异常的对象，将其打印出来
    print("Other error occur: ", e)
except: # 其它非Exception的异常走此流程，例如Ctrl+C中止程序的异常，他的基类是BaseException
    print("not Exception occur")
else: # 如果没有异常，会执行此语句块
    print("Calculate complete")
finally: # 无论是否有异常，都会执行此语句块，一般用于做清理工作，例如连接的关闭等
    print("clear")