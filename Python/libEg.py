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