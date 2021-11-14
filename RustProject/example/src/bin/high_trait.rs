use std::fmt::Formatter;

fn main() {
    // 标准库中Iterator迭代器trait的声明，内有关联类型Item
    // next返回的类型是Option<Self::Item>类型，即返回Item指定的类型
    pub trait Iterator {
        type Item;
        fn next(&mut self) -> Option<Self::Item>;
    }

    // 定义一个无限计数器，将关联类型指定成i32，返回一个Some(i32)
    struct Counter(i32);
    impl Iterator for Counter {
        type Item = i32;
        fn next(&mut self) -> Option<Self::Item> {
            self.0 += 1;
            Some(self.0)
        }
    }

    // 如果使用泛型来实现就显得很奇怪
    pub trait Iterator2<T> {
        fn next(&mut self) -> Option<T>;
    }

    // 在指定实现泛型trait时，需要显示指定实现的版本是什么
    // 如果有多个类型的实现，在调用的时候还要指定调用的是哪个版本的实现
    // 而使用关联类型则可以保证只有一种实现
    impl Iterator2<i32> for Counter {
        fn next(&mut self) -> Option<i32> {
            self.0 += 1;
            Some(self.0)
        }
    }

    // 默认泛型参数
    // 公共库中的Add trait实现加法运算符的重载，默认类型与被加数(Right-hand side)相同(Rhs = Self)
    pub trait Add<Rhs = Self> {
        type Output;
        fn add(self, rhs: Rhs) -> Self::Output;
    }
    #[derive(Debug, PartialEq)]
    struct Point(i32, i32);
    // 为Point实现了Add trait，没有指定泛型的类型，默认是Self，即Point类型
    // 等价于impl std::ops::Add<Point> for Point
    impl std::ops::Add for Point {
        type Output = Point;

        fn add(self, rhs: Point) -> Self::Output {
            Point(self.0 + rhs.0, self.1 + rhs.1)
        }
    }
    assert_eq!(Point(1, 2) + Point(2, 3), Point(3, 5));

    // 为非默认类型(i32)实现Add操作
    impl std::ops::Add<i32> for Point {
        type Output = Point;
        fn add(self, rhs: i32) -> Self::Output {
            Point(self.0 + rhs, self.1 + rhs)
        }
    }
    assert_eq!(Point(1, 2) + 2, Point(3, 4));


    // 有两个同名函数的trait，以及同时实现这两个trait的类型在调用时发生的歧义调用
    trait Pilot {
        fn fly(&self) {
            println!("Engine start!")
        }
        fn name() {
            println!("Pilot")
        }
    }

    trait Wizard {
        fn fly(&self) {
            println!("Up!")
        }
        fn name() {
            println!("Wizard")
        }
    }
    struct Human {}
    impl Pilot for Human {}
    impl Wizard for Human {}
    impl Human {
        fn fly(&self) {
            println!("Dreaming!")
        }
        fn name() {
            println!("Human")
        }
    }
    // Human实现了Wizard的fly、Pilot的fly、以及自己也有fly方法
    let h = Human {};
    // 此时在调用fly时，会直接使用类型定义的方法
    // 如果Human没有实现自己的fly，此时将出现编译错误：multiple `fly` found
    h.fly(); // Dreaming!
    // 可以使用全限定的语法来指定调用哪一个fly方法
    Human::fly(&h); // Dreaming!
    Wizard::fly(&h); // Up!
    Pilot::fly(&h); //Engine start!

    // 对于没有self参数的函数冲突，原来的办法无能为力
    // 以下报错：cannot infer type
    // note: cannot satisfy `_: Wizard`
    // Wizard::name();
    // Human可以直接调用，输出Human的方法内容
    Human::name(); // Human
    // trait不能直接调用其函数，需要使用以下的限定语法
    <Human as Pilot>::name(); // Pilot
    <Human as Wizard>::name(); // Wizard

    // 超trait，依赖于另外一个trait的trait
    trait Animal {
        fn class(&self) {}
    }
    // 依赖于类型实现了Animal，因为需要调用其class方法
    trait Cat: Animal {
        fn miao(&self) {
            self.class()
        }
    }
    struct Persian {}
    // Persian必须实现了Animal，才能实现Cat
    impl Animal for Persian {}
    impl Cat for Persian {}

    // newtype模式
    // 绕过孤儿规则，为Vec<String>实现Display trait
    struct Wrapper<'a>(Vec<&'a str>);
    impl<'a> std::fmt::Display for Wrapper<'a> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "[{}]", self.0.join(","))
        }
    }
    let w = Wrapper(vec!["i", "am", "supper", "man"]);
    println!("{}", w); // [i,am,supper,man]
    // 但是封装类型并没有实现所有Vec<&str>的能力，比如len函数，w.len()将报错，因为没有实现
    // 此时可以使用Deref解引用trait以及自动解引用的性质来实现Wrapper与Vec<&str>更一致
    impl<'a> std::ops::Deref for Wrapper<'a> {
        type Target = Vec<&'a str>;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }
    println!("len:{}", w.len());
    std::process::exit(1)
}