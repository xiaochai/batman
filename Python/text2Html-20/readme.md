使用方法：cat eg.txt| python3 markup.py  > eg.html

原理：

1. 先将文本分块，按空行划分
2. 对所有块运用filter，即次星号的添加em标签，url的添加a标签等
3. 对所有块运行ruler，命中的ruler调用action方法
4. 所有ruler的action方法是直接输出对应标签包裹的内容，对和ul和li这样的，需要一些技巧处理一下。