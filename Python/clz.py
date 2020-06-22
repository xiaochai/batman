# 类的创建和基本使用
# 这个self是约定命名，可以是任何变量名
class Person:
    def set_name(self, name):
        self.name = name
    def get_name(self):
        return self.name
    def greet(self):
        print("Hello, world! I'm {}.".format(self.name))
p1 = Person()
p1.set_name("Jonh")
p2 = Person()
p2.set_name("Lee")
p1.greet() # Hello, world! I'm Jonh.
p2.greet() # Hello, world! I'm Lee.
print(p1.get_name()) # Jonh

# 可以直接对属性进行设置
p1.name = "Mark"
print(p1.name) # Mark

# 类方法的调用，也可以使用类名的方式调用，我理解在类上的调用应该与此等价
Person.greet(p1) # Hello, world! I'm Mark.


# Python中没有私有属性，可以通过以下方法来达到私有属性的效果
class Sec :
    __name = "J" # 两个下划线开头的属性或者方法，实际上都会添加下划线开头加类名的前缀
    def set_name(self, name):
        self.__name = name
    def get_name(self):
        return self.__name
    def greet(self):
        print("Hello, world! I'm {}.".format(self.__name))

s = Sec()
s.greet() # Hello, world! I'm J.
s.__name = "K"
s.greet() # Hello, world! I'm J.
s._Sec__name = "K" # 双下划线开头的会添加前缀，实际中不推荐这样使用
s.greet() # Hello, world! I'm K.
# 也可以对一个不存在的属性赋值
s.__k  = "s"

# 单下划线也被视为不能直接修改约定，在使用from module import *中不会导入以下划线开头的名称

# 类名空间下的成员是所有对象共享的，也是独立于对象的，如果对象的属性没有定义，则使用类的属性
print(Sec._Sec__name) # J
# 所以类也可以被当成命名空间来使用，因为在类中可以直接执行语句
class C:
    print("Class C being defined")
# 以上输出 Class C being defined


# 继承的基本使用
class Base:
    def init(self):
        self.p = "base"
    def show(self):
        print("I'm ", self.p)

# 在定义类时添加括号表示继承的基类
class Extend(Base):
    def init(self):
        self.p = "extend"

b = Base()
b.init()
b.show() # I'm  base
e = Extend()
e.init()
e.show() # I'm  extend

# 与继承相关的方法
# 是否是继承关系
print(issubclass(Extend, Base)) # True
# 获取基类，由于支持多重继承，所以还一个属性__bases__
print(Extend.__base__) # <class '__main__.Base'>
# object是所有类的基类
print(Base.__base__) # <class 'object'>

# 以下两个都返回True，判断对象是否是某个类
print(isinstance(Extend(), Base))
print(isinstance(Extend(), Extend))
# 获取对象属于哪个类
print(Extend().__class__) # <class '__main__.Extend'>

# 多重继承
# 如果写成Super(Base, Extend)则无法运行，因为不满足MRO，可阅读参考文献
# 所以在多重继承中注意两个超类含有同一个属性或者方法的情况
class Super(Extend, Base): pass
s = Super()
s.init()
s.show() # I'm  extend


# 判断某一个对象是否有某一个属性，以及这个属性是否可调用
print(hasattr(s, "show")) # True
# getattr的第三个参数表示不存在时的默认值，如果不提供默认值且不存在，则报错
print(getattr(s, "p")) # extend
# callable检查是否可调用
print(callable(getattr(s, "show", None))) # True
# 设置对象的属性，返回None
setattr(s, "p1", "p1val")
# 获取所有属性
print(s.__dict__) # {'p': 'extend', 'p1': 'p1val'}

# 抽象基类
# 在Python中并没有提供原生的语法来支持抽象类，但可以使用abc这个官方模块解决此问题
from abc import ABC, abstractclassmethod
class Talker(ABC):
    @abstractclassmethod
    def talk(self): pass
# 以下语句会报错TypeError: Can't instantiate abstract class Talker with abstract methods talk
# t = Talker()
# 如果只是继承了而没有实现抽象方法，还是会报错
class Foo(Talker): pass
# t = Foo()
class Knigget(Talker):
    def talk(self):
        print("Ni")
k = Knigget()
# 一般的抽象类应用场景中会有isinstance判断
if isinstance(k, Talker):
    k.talk() # Ni
# 但这与Python的鸭子类型的编程思想，如果一个类有talk方法，但没有继承Talker，则在此场景中没有办法调用talk

# register提供了将某一类注册为另外一个类子类的办法
class Herring:
    def talk(self):
        print("Blue")
h = Herring()
print(isinstance(h, Talker)) # False
# 将Herring 注册为Talker的子类
Talker.register(Herring)
print(isinstance(h, Talker)) # True
print(issubclass(Herring, Talker)) # True

# 这种做法的问题是，如果Herring没有实现talk方法，则此时调用talk方法会直接报错，如下：
class Clam: pass
Talker.register(Clam)
c = Clam()
if isinstance(c, Talker):
    c.talk() # 这里将报错，因为Clam没有实现talk方法，但又是Talker的子类
