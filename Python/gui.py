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
