use std::ops::{Deref, DerefMut};

fn main() {
    // Box的基本使用方法
    // a和b都是Box<i32>类型，与i32不同的是，Box<i32>类似于指向堆上两个i32类型的指针
    let a = Box::new(128);
    let b = Box::new(256);
    // 所以a和b无法直接进行操作，因为Box实现了Deref这一trait，所以可以使用*来解引用
    let c: i64 = *a + *b;
    println!("{}", c); // 384

    // 递归类型必须使用Box
    enum List {
        Cons(i32, Box<List>),
        // 使用Cons(i32, List)将无法通过编译：recursive type `List` has infinite size
        Nil,
    }

    use List::{Nil, Cons};

    let list = Cons(
        1,
        Box::new(Cons(
            2,
            Box::new(Cons(
                3, Box::new(
                    Nil
                ),
            )),
        )),
    );

    fn print_list(l: List) {
        match l {
            Cons(i, nl) => {
                print!("{}=>", i);
                print_list(*nl);
            }
            Nil => {
                println!("end");
            }
        }
    }
    print_list(list);

    // 善于Box运行机制的实验，对于没有实现Copy trait的，以下通不过，所以Box::new()中的参数是不是一般都是字面量？
    struct My {
        age: i32,
    }
    let t = My { age: 10 };
    // 报错 borrow of moved value: `t`
    // let b = Box::new(t);
    println!("{}", t.age);

    // 自定义类似于Box的智能指针(但数据存在在栈上)
    struct MyBox<T> (T); // 元组结构体
    impl<T> MyBox<T> {
        // 提供一个参数，并将此存入结构体中
        fn new(x: T) -> MyBox<T> {
            MyBox(x)
        }
    }

    // 为MyBox实现Deref，这样就可以使用解引用运算符
    impl<T> Deref for MyBox<T> {
        // 关联类型
        type Target = T;

        // 也可以写成fn deref(&self) -> &T {
        fn deref(&self) -> &Self::Target {
            // 以下等价于self.0
            match self {
                MyBox(i) => i
            }
        }
    }
    let a = MyBox(10);
    // 这里的*a类似于*(a.deref())，称之为隐式展开
    println!("{}", *a + 10);

    // 隐式解引用转换
    fn hello(name: &str) {
        println!("hello, {}", name);
    }
    let mb: MyBox<String> = MyBox::new("lee".to_string());
    // 正常的写法如下，其发生的步骤1. 需要将MyBox解引用转化为String，2. 使用[..]将String转化为切片&str，这样才符合参数要求
    hello(&(*mb)[..]);
    // 使用自动解引用也能满足这个要求，mb实现了Deref，所以可以获取到String，String也实现了deref，其返回为&str，所以编译器进行了两次解引用转换
    // 以上是编译期完成，不会有任何运行时开销
    hello(&mb);

    // 实现可变解引用运算符，实现DerefMut之前必须实现Deref
    impl<T> DerefMut for MyBox<T> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.0
        }
    }
    // 满足自动解引用的三条规则
    // 当T: Deref<Target=U>时，允许&T转换为&U。
    // 当T: DerefMut<Target=U>时，允许&mut T转换为&mut U。
    // 当T: Deref<Target=U>时，允许&mut T转换为&U。
    fn hello_exp(name:&mut String) -> &mut String {
        name.push_str(", hello!");
        name
    }

    let mut b = Box::new("abc".to_string());
    // 当T: DerefMut<Target=U>时，允许&mut T转换为&mut U，与&mut *b是一样的
    let c = hello_exp(&mut b);
    println!("{}", c);
    //  当T: Deref<Target=U>时，允许&mut T转换为&U。
    hello(&mut b)
}