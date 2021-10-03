use std::ffi::CString;

const MAX_SCORE: i32 = 199;

fn main() {
    // 变量默认不可变
    let x: i32;
    x = 5;
    let y: i32 = 10;
    // 以下行报错Cannot assign twice to immutable variable [E0384]
    // y=9

    // 使用mut修饰可变变量
    let mut x = 10;
    x = 100;
    x = 1000;

    // 常量
    const MY_SCORE: i32 = 200;
    const MAX_SCORE: i32 = 299;
    println!("{},{} ", MY_SCORE, MAX_SCORE);

    // 变量隐藏
    let space = "   ";
    let space = space.len();

    // 以下报错：type annotations needed
    // let k = ("32").parse().expect("not ok");
    let k: i32 = ("32").parse().expect("not ok");

    // 整数类型字面量
    // 10进制，可以用下划分分隔
    let x: u32 = 98_000;
    let x: u32 = 0xff; // 十六进制
    let x: u32 = 0o77; // 八进制
    let x: u32 = 0b1111_0000; // 二进制
    let x = b'A'; // u8

    // 整数溢出
    let x: u8 = 252;
    let y: u8 = ("32").parse().expect("not ok");
    // 在debug模式编译时，会在运行进报错
    // thread 'main' panicked at 'attempt to add with overflow', src/main.rs:41:26
    // note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace
    // 在release模式下，则不会报错，输出28
    // println!("{},see!", x + y);

    // 浮点
    let x = 22.3;
    let x: f32 = 1.1;

    // 类型不一样，无法操作
    // let x:u8 = 10;
    // let y:u32 = 100;
    // let z = y+x;

    let x: bool = false;
    // false
    println!("{}", x);


    // 字符类型
    let x = 'c';
    let y = '李';
    let z: char = '李';

    // 元组
    // 也可以省略类型，让编译器推断
    let x: (i64, f64, char) = (2, 3.4, 'c');
    // 使用模式匹配来解构元组
    let (a, b, c) = x;
    // 通过点来访问元组
    println!("{},{},{}", x.0, x.1, x.2);


    // 数组
    // 声明一个长度为5，元素为int32类型的数组；类型[i32;5]也可以省略，由编译器推断
    let x: [i32; 5] = [1, 2, 3, 4, 5];
    // 定义了一个由10个1组成的数组，即[1,1,1,1,1,1,1]
    let y = [1; 7];
    // 通过下标来访问，从0开始；以下输出3,1；下标的类型是usize的
    println!("{},{}", x[2], y[6]);
    // 越界时将发生严重错误
    // 编译时报错：index out of bounds: the length is 5 but the index is 10
    // 如果下标是运行时才确定的值，则这块的报错将在运行时报错
    // let k:usize = "10".parse().expect("not a number");
    // println!("{}", x[k]);

    // 语句，没有返回值
    let x = 6;
    // 所以不能将语句赋值给变量
    // let y = (let x = 7);
    // 以下也不行
    // let a = b = 6;
    // 花括号的代码块也是表达式，以下表达式的值为11
    let x = {
        let y = 10;
        // 注意这一行不能有分号，添加了分号后，这个代码块的值就是空元组了()
        y + 1
    };

    println!("{}", my_sum(1, 3));

    // if的用法，多分支判断，条件表达式的值只能是bool类型
    let num = 100;
    if num % 4 == 0 {
        println!("number is divisible by 4")
    } else if num % 3 == 0 {
        println!("number is divisible by 3")
    } else {
        println!("number is not divisible by 3,4")
    }

    // if为表达式，所以可以使用if来赋值
    // 此处要注意各个分支返回的数据类型要一样，否则编译期直接报错
    let condition = true;
    let x = if condition {
        10
    } else {
        20
    };

    // loop表达式的值，可以从break返回
    let mut i = 1;
    let count = loop {
        i += 1;
        if i > 10 {
            break i;
        }
        // 两个break的值类型要一样，所以以下这一行无法通过编译
        // break 'a'
    };

    // while表达式的值一直是空值，如果使用break，后面不能跟返回值
    let mut i = 1;
    let count: () = while i > 10 {
        i += 1;
    };

    // 数组使用for循环打印，x.iter()返回迭代器
    let x = [1, 2, 3];
    for e in x.iter() {
        println!("{}", e)
    }
    // y是一个Range<i32>类型，本身是一个迭代器；值为1,2(不包括3)
    // rev是从后往前遍历，所以这块打印的是2,1,
    let y = 1..3;
    for e in y.rev() {
        print!("{},", e)
    }

    println!();

    println!("celsius_2_fahrenheit(1):expect{}, actual:{}", 33.8, celsius_2_fahrenheit(1.0));
    println!("fibonacci(10):expect{}, actual:{}", 34, fibonacci(10));
    the_six_days_of_christmas();


    // 字符串类型在堆上分配
    // String::from方法，使用字符串字面量来创建String类型，这里s必须是可变的
    let mut s = String::from("Hello");
    s.push_str(", world");
    println!("{}", s);

    let s1 = String::from("Hello");
    let s2 = s1;
    // 以下代码不能通过编译，borrow of moved value: `s1`
    // s1已经被移动到s2中，无法再借用
    // println!("{}", s1);

    // 对s1进行深拷贝，即将堆上的内容也进行了拷贝
    let s1 = String::from("Hello");
    let s2 = s1.clone();
    println!("{}, {}", s1, s2);

    // 如果这个元组加了String类型，如下，则不能通过编译
    // let x = (1,2,3.0, String::from("Hello"));
    let x = (1, 2, 3.0);
    let y = x;
    println!("{}", x.1);

    let x = [1, 2, 3];
    let y = x;
    println!("{}", x[1]);


    fn take_owner(s: String) {}
    let s = String::from("hello");
    take_owner(s);
    // 将发生编译错误，因为s的所有权移进了take_owner中
    // println!("{}", s);

    // 参数获取了s的所有权
    fn take_owner_and_return(s: String) -> String {
        // 函数又将s的所有权转移到返回值，所以s不会被drop掉
        return s;
    }
    let s1 = String::from("hello");
    let s2 = s1;// 到这里，s1已经失效
    let s3 = take_owner_and_return(s2); // 到这里s2已经失效
    // 这里只有s3可用，其它的s1,s2失效

    // 引用传递时，并不取得所有权，但可以使用值
    fn get_len(s: &String) -> usize {
        // s是一个不可变引用，所以无法对s的值进行改变
        // 以下无法通过编译
        // s.push_str(", world");
        return s.len();
    }
    let s1 = String::from("hello");
    // s2为可变引用，默认的引用不可变
    let mut s2: &String = &s1;
    let mut s3 = String::from("hello");
    // 注意s2为可变引用的意思是可以改变s2引用到哪个值，而不能改变s2引用的值
    // 以下无法通过编译， 报错: cannot borrow `*s2` as mutable, as it is behind a `&` reference
    // s2.push_str(", world");
    // 因为两行可以运行，因为s2是可变引用，而且s3是可变String
    s2 = &mut s3;
    // 虽然s2引用了可变的s3，但由于s2的类型是&String不是&mut String，所以到下这一行还是无法通过编译
    // s2.push_str(", world");
    // 获取s1的长度，而不取得s1的所有权
    let size = get_len(&s1);
    {
        let s4 = String::from("you");
        // 由于s4的生命周期比s2短，所以s2无法引用s4，这避免了悬垂指针的出现
        // s2 = &s4;
    }
    // 在这里s1还是能用
    println!("{},{},{},{}", s1, s2, size, s1);


    let mut s: String = String::from("hello");
    let s1: &mut String = &mut s;
    s1.push_str(",world");

    let mut s: String = String::from("hello");
    let mut s2: &String = &mut s;
    // 以下无法通过编译，因为s2是&String不可变类型
    // s2.push_str(", world");

    let mut s = String::from("hello");
    let s1 = &mut s;
    // cannot borrow `s` as mutable more than once at a time
    // let s2 = &mut s;
    // cannot borrow `s` as immutable because it is also borrowed as mutable
    // let s3 = &s;
    println!("{},{}, {}", s1, s2, s3);


    // 字符串切片，类似与go，获取[begin,end)之间的内容，注意end最大是字符串的长度，超过后会报错
    let s = String::from("0123456789");
    // 以下两个hello和world等价
    let hello = &s[0..5];
    let hello: &str = &s[..5];
    let world = &s[5..10];
    let world = &s[5..];
    // 01234,56789
    println!("{},{}", hello, world);

    // 获取第一个单词，返回字符串切片
    // 这里的函数参数也可以使用&str，可以更通用
    fn first_world(s: &String) -> &str {
        // s.as_bytes()将字符串转成字节数组&[u8]
        // iter返回迭代器，enumerate将每一个元素按元组的形式返回
        for (i, &item) in s.as_bytes().iter().enumerate() {
            // 判断是空格，直接返回切片
            if item == b' ' {
                return &s[0..i];
            }
        }
        &s[..] // 使用&s也可以
    }
    // 输出hello
    println!("{}", first_world(&String::from("hello world")));

    let mut s = String::from("0123456789");
    let t = first_world(&s);
    // 以下无法通过编译，因为s已经是不可变引用了，s.clear又使用了可变引用
    // cannot borrow `s` as mutable because it is also borrowed as immutable
    // s.clear();
    println!("{},{}", s, t);

    // 其它切片
    let ia = [1, 2, 3, 4, 5, 6];
    let sia: &[i32] = &ia[1..3];

    //////////// 结构体
    // 结构体定义
    struct User {
        username: String,
        email: String,
        sign_in_count: u64,
        active: bool,
    }
    // 创建实例
    let mut user1 = User {
        username: String::from("xiaochai"),
        email: "soso2501@mgail.comxxx".to_string(),
        sign_in_count: 1,
        active: true,
    };
    println!("{}", user1.email); // soso2501@mgail.comxxx
    // 访问和修改，注意一旦实例可变，则实例的所有成员都可变
    user1.email = String::from("soso2501@mgail.com");
    println!("{}", user1.email); // soso2501@mgail.com

    fn build_user(email: String, username: String) -> User {
        User {
            username, // 由于变量名了字段同名，所以可以省略掉字段名
            email,
            sign_in_count: 1,
            active: true,
        }
    }
    let mut user1 = build_user("soso2501@mgail.com".to_string(), "xiaochai".to_string());
    let mut user2 = User {
        username: "xiaochai2".to_string(),
        // 可以使用以下语法从user1复制剩下的字段
        ..user1
    };
    user1.email = "sosoxm@163.com".to_string();
    // soso2501@mgail.com,sosoxm@163.com
    println!("{},{}", user2.email, user1.email);

    // 元组结构体
    // 当成员变量没有名字时，结构体与元组类似，称为元组结构体
    struct Point(u32, u32, u32);
    let origin = Point(0, 0, 0);
    // 可以使用数字下标来访问
    println!("{},{},{}", origin.0, origin.1, origin.2);
    // 也可以通过模式匹配来结构
    let Point(x, y, z) = origin;
    println!("{},{},{}", x, y, z);

    // 空结构体，一般用于trait
    struct Empty {}

    // 如果结构体的成员是引用时，需要带上生命周期的标识
    struct User2<'a> {
        username: &'a str,
    }

    // 使用结构的例子，说明trait的使用
    #[derive(Debug)] // 添加注解来派生Debug trait
    struct Rectangle {
        width: u32,
        height: u32,
    }
    fn area(rect: &Rectangle) -> u32 {
        rect.width * rect.height
    }
    let rect1 = Rectangle { width: 10, height: 20 };
    // {:?}需要结构体实现Debug这一trait，也可以使用{:#?}来分行打印
    // the area of rectangle Rectangle { width: 10, height: 20 } is 200
    println!("the area of rectangle {:?} is {}", rect1, area(&rect1));

    // 为结构体定义方法
    impl Rectangle {
        // 方法的第一个参数永远是self
        fn area(&self) -> u32 {
            self.height * self.width
        }
    }
    println!("the area of rectangle {:?} is {}", rect1, rect1.area());

    impl Rectangle {
        // 以下无法通过编译，因为area重复定义了
        // fn area(&self, i: i32) -> u32 {
        //     self.height * self.width
        // }
        fn can_hold(&self, rect2: &Rectangle) -> bool {
            self.width > rect2.width && self.height > rect2.height
        }
        // 关联函数
        fn new(width: u32, height: u32) -> Rectangle {
            Rectangle {
                width,
                height,
            }
        }
    }
    println!("the area of rectangle {:?} is {}", Rectangle::new(3, 2), Rectangle::new(2, 3).area());

    enum IPAddr {
        // 变体中可以保存数值
        IPV4(u32, u32, u32, u32),
        IPV6(String),
    }
    // 可以为枚举定义函数
    impl IPAddr {
        fn print(&self) {
            // 使用match来处理每一种变体，注意需要处理所有变体，否则编译保险错
            match self {
                // 模式匹配可以直接解构变体内的数值
                IPAddr::IPV4(u1, u2, u3, u4) =>
                    println!("{}.{}.{}.{}", u1, u2, u3, u4),
                IPAddr::IPV6(s) => println!("{}", s),
            }
        }
    }
    let ipv4 = IPAddr::IPV4(1, 2, 3, 4);
    let ipv6 = IPAddr::IPV6("::1".to_string());
    ipv4.print();
    ipv6.print();

    // 标准库中的Option类型
    // 为一个Option值加1
    fn plus_one(c: Option<i32>) -> Option<i32> {
        match c {
            None => None,
            Some(i) => Some(i + 1)
        }
    }
    let five = plus_one(Some(4));
    let none = plus_one(None);
    match five {
        None => println!("none"),
        Some(i) => println!("val is {}", i)
    };
    match none {
        None => println!("none"),
        Some(i) => println!("val is {}", i)
    };

    // 必须匹配每一个可能的值
    let c = 2;
    match c {
        1 => println!("is 1"),
        2 => println!("is 2"),
        // 如果没有以下这一行，则编译报错
        // non-exhaustive patterns: `i32::MIN..=0_i32` and `3_i32..=i32::MAX` not covered
        _ => println!("other")
    }

    // 使用if let来简化处理
    let five: Option<i32> = Some(5);
    if let Some(i) = five {
        println!("has value {}", i)
    }
}

fn my_sum(x: i32, y: i32) -> i32 {
    // 可以直接是return，也可以直接写表达式x+y，函数中将最后一个表达式的值做为返回值
    return x + y;
}


// 摄氏温度与华氏温度的相互转换
// println!("celsius_2_fahrenheit(1):expect{}, actual:{}", 33.8, celsius_2_fahrenheit(1.0));
fn celsius_2_fahrenheit(celsius: f64) -> f64 {
    celsius * 1.8 + 32.0
}

// 生成一个n阶的斐波那契数列
// println!("fibonacci(10):expect{}, actual:{}", 34, fibonacci(10));
fn fibonacci(n: i64) -> i64 {
    if n == 1 {
        0
    } else if n == 2 {
        1
    } else {
        fibonacci(n - 1) + fibonacci(n - 2)
    }
}

// 打印圣诞颂歌The Twelve Days of Christmas的歌词，并利用循环处理其中重复的内容。
// 太长了，改成The Six Days of Christmas
// the_six_days_of_christmas();
fn the_six_days_of_christmas() {
    let num_map = ["first", "second", "third", "forth", "fifth", "sixth"];
    let gifts = ["a partridge in a pear tree",
        "two turtle doves",
        "three French hens",
        "four calling birds",
        "five golden rings",
        "six geese a-laying",
    ];

    for i in 0..6 {
        print!("On the {} day of Christmas, my true love sent to me:", num_map[i]);
        let mut j = i;
        while j > 0 {
            print!("{},", gifts[j]);
            j -= 1;
        }
        // 最后一个礼物不需要逗号
        println!("{}.", gifts[0]);
    }
}

