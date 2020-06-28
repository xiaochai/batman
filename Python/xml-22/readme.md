分析xml文件来生成网站的结构，使用到了XML的解析库Simple API for XML(SAX)，它使用事件的方式来处理遇到的tag。

运行```python3 website.py```后会生成public_html目录，并生成对应配置的子目录。

除了使用SAX这种基于事件的模式外，还可以使用DOM将整体XML解析成文档树的形式整体分析，这种做法会更容易操作文档结构，但也会占用更多的内存。

