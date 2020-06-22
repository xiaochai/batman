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