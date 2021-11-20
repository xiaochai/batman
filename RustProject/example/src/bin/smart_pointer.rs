use std::borrow::Borrow;
use std::cell::RefCell;
use std::ops::{Deref, DerefMut};
use std::rc::{Rc, Weak};

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
    fn hello_exp(name: &mut String) -> &mut String {
        name.push_str(", hello!");
        name
    }

    let mut b = Box::new("abc".to_string());
    // 当T: DerefMut<Target=U>时，允许&mut T转换为&mut U，与&mut *b是一样的
    let c = hello_exp(&mut b);
    println!("{}", c);
    //  当T: Deref<Target=U>时，允许&mut T转换为&U。
    hello(&mut b);


    // 实现Drop的例子
    {
        struct TestDropStruct {
            data: String,
        }
        impl Drop for TestDropStruct {
            fn drop(&mut self) {
                println!("{}", self.data)
            }
        }
        let _a = TestDropStruct { data: "first object".to_string() };
        let _b = TestDropStruct { data: "second object".to_string() };
        let _c = TestDropStruct { data: "third object".to_string() };
        println!("main end");
        // 手动使用std::mem::drop函数来提前清理_c的值
        drop(_a);
        // 没有使用的变量直接drop
        TestDropStruct { data: "no left val".to_string() };
        println!("real main end！！！！！！！！");

        // 以上输出顺序为
        // main end
        // first object
        // no left val
        // real main end！！！！！！！！
        // third object
        // second object
        // 对于_b，_c来说，丢弃顺序与创建顺序相反，所以_c先调用drop函数
        // _a则是由于手动释放导致先执行
    }


    // 为MyBox实现Drop trait
    impl<T> Drop for MyBox<T> {
        // drop函数的参数是自己的可变引用
        fn drop(&mut self) {
            // println!("my drop:")
        }
    }

    {
        // 测试引用计数智能指针Rc<T>类型
        enum RcList {
            RcCons(i32, Rc<RcList>),
            RcNil,
        }
        use RcList::*;
        // 新建一个引用记录类型使用Rc::new来创建，此处a为Rc<RcList>类型
        let a = Rc::new(RcCons(3, Rc::new(
            RcCons(4, Rc::new(RcNil)),
        )));
        {
            // Rc::clone全程参数对应的引用计数增加1
            let _b = RcCons(1, Rc::clone(&a));
            let _c = RcCons(2, Rc::clone(&a));
            // 目前有a,_b,_c三个变量引用a，所以a的引用计数为3
            // 除了strong_count，还有week_count，用于避免循环引用
            println!("{}", Rc::strong_count(&a))
        }
        // _b，_c离开作用域，减少了引用计数，但a还在，所以这块的数量是1
        println!("{}", Rc::strong_count(&a))
    }
    {
        // RefCell的试用
        // 假设有一个消息trait，需要实现send方法，而这一方法传递self的不可变引用
        pub trait Messenger {
            fn send(&self, msg: &str);
        }

        // 但是在测试的时候，我们使用Mock类来模拟Messenger的send的时候，需要把消息存储下来
        // 这样就可以验证存下来的消息是否符合预期，但传入不可变的self阻止了这种做法
        // 此时就需要使用RefCell来实现
        struct Mock {
            message: RefCell<Vec<String>>,
        }
        impl Mock {
            fn new() -> Mock {
                Mock {
                    // 使用RefCell::new来创建新的RefCell
                    message: RefCell::new(vec![])
                }
            }
        }
        impl Messenger for Mock {
            fn send(&self, msg: &str) {
                // 如果这里的self.message是普通的Vec<String>，则无法执行push方法
                // 而使用RefCell的borrow_mut来绕过此限制，得到可变引用
                self.message.borrow_mut().push(String::from(msg))
            }
        }
        let m = Mock::new();
        m.send("message1");
        m.send("message2");
        // ["message1", "message2"]
        println!("{:?}", m.message.borrow());
        // 以下两行可以通过编译，但在运行时报错：thread 'main' panicked at 'already borrowed: BorrowMutError'
        // 这是因为a是不可变引用，在已经持有不可变引用的情况下，又搞出了一个可变的引用，破坏了借用规则 ，所以panic
        // let a = m.message.borrow();
        // let mut b = m.message.borrow_mut();
    }
    {
        // RefCell与Rc合并使用，创建出多重引用，并且可以修改他的值
        #[derive(Debug)]
        enum RcRefCellList {
            RcRefCellCons(Rc<RefCell<i32>>, Rc<RcRefCellList>),
            RcRefCellNil,
        }
        use RcRefCellList::*;
        let val = Rc::new(RefCell::new(5));
        let a = Rc::new(RcRefCellCons(Rc::clone(&val), Rc::new(RcRefCellNil)));
        let b = RcRefCellCons(Rc::new(RefCell::new(6)), Rc::clone(&a));
        let c = RcRefCellCons(Rc::new(RefCell::new(6)), Rc::clone(&a));
        // 这块改动将a，b和c以及val都修改了
        *val.borrow_mut() = 10;
        // a:RcRefCellCons(RefCell { value: 10 }, RcRefCellNil)
        // b:RcRefCellCons(RefCell { value: 6 }, RcRefCellCons(RefCell { value: 10 }, RcRefCellNil))
        // c:RcRefCellCons(RefCell { value: 6 }, RcRefCellCons(RefCell { value: 10 }, RcRefCellNil))
        println!("a:{:?}", a);
        println!("b:{:?}", b);
        println!("c:{:?}", c);
    }

    {
        // 创建出循环引用
        // 定义一个链接，为了方便改动，这次的链接使用ReCell来处理
        enum RefCellRcList {
            RefCellRcCons(i32, RefCell<Rc<RefCellRcList>>),
            RefCellRcNil,
        }
        use RefCellRcList::*;
        // 定义一个a，指向b
        // a -> 5 -> Nil
        let a = Rc::new(
            RefCellRcCons(5, RefCell::new(Rc::new(RefCellRcNil))),
        );
        println!("reference count:a:{}", Rc::strong_count(&a));

        // b -> 10 -> a
        let b = Rc::new(
            RefCellRcCons(10, RefCell::new(Rc::clone(&a))),
        );
        println!("reference count:a:{}, b:{}", Rc::strong_count(&a), Rc::strong_count(&b));
        // 将a -> b；最终变成a->b->10->a
        if let RefCellRcCons(i, r) = a.borrow() {
            *r.borrow_mut() = Rc::clone(&b);
        };
        println!("reference count:a:{}, b:{}", Rc::strong_count(&a), Rc::strong_count(&b));
        // reference count:a:1
        // reference count:a:2, b:1
        // reference count:a:2, b:2
        // 此时a和b的引用计数都是2，在结束时，先释放b，将b的引用计数减1，但此时b已经无法将引用计数减成0了，所以无法释放
    }

    {
        // 使用Weak<T>创建树结构，子节点指向父节点使用弱引用
        #[derive(Debug)]
        struct Node {
            val: i32,
            // 多个子节点使用Vec来保存子节点的强引用，RefCell方便修改对应的值
            children: RefCell<Vec<Rc<Node>>>,
            // 父节点点使用弱引用
            parent: RefCell<Weak<Node>>,
        }

        let leaf = Rc::new(Node {
            val: 10,
            children: RefCell::new(vec![]),
            // Weak::new()创建出一个空的弱引用
            parent: RefCell::new(Weak::new()),
        });
        // 弱引用获取时，由于不确定值是否回收，所以使用upgrade()时会返回Option<T>，如果已经回收或者没有值，返回None
        // 以下返回None
        println!("{:?}", leaf.parent.borrow().upgrade());

        let branch = Rc::new(Node {
            val: 5,
            children: RefCell::new(vec![Rc::clone(&leaf)]),
            parent: RefCell::new(Weak::new()),
        });
        *(leaf.parent.borrow_mut()) = Rc::downgrade(&branch);
        println!("leaf's parent:{:?}", leaf.parent.borrow().upgrade());
        println!("branch's parent:{:?}", branch.parent.borrow().upgrade());
        {
            // 在另外一个作用域中添加branch的parent
            let new_branch = Rc::new(Node {
                val: 6,
                children: RefCell::new(vec![Rc::clone(&branch)]),
                parent: RefCell::new(Weak::new()),
            });
            *branch.parent.borrow_mut() = Rc::downgrade(&new_branch);
            println!("branch's parent in new area:{:?}", branch.parent.borrow().upgrade());
        }

        // 由于new_branch离开了作用域，所以被销毁，这块拿到的是None
        println!("branch's parent out area:{:?}", branch.parent.borrow().upgrade());
    }
}