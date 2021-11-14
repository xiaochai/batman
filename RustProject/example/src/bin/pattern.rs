fn main() {
    // match匹配模式
    // 必须穷尽所有的可能性；为了满足这一条件，可以使用两种方式
    // 1. 最后一个分支使用全匹配模式，可以使用一个变量名来处理
    // 2. 使用下划线这一特殊的模式来匹配所有值
    let m = 5;
    match m {
        // 字面量匹配
        1 => println!("is 1"),
        // 变量名匹配
        x => println!("is not 1, is {}", x)
    }
    match m {
        // 匹配区间值
        // 在老的版本中1..=5的语法也写作1...5，但最新的rust 2021只能使用1..=5，表示1~5包含1和5
        // 也可以使用|来表示，等价于1|2|3|4|5，但写起来更简单，字符类型也可以使用，例如'a'..='k'
        1..=5 => println!("between 1~5"),
        _ => println!("others")
    }

    // if let条件表达式
    // if let表达式只匹配match中的一种情况
    // 可以与if, else if, else, else if let等混合使用，以下是一个例子
    let favorite_color: Option<&str> = None;
    let is_friday = false;
    let age: Result<u8, _> = "34".parse();
    if let Some(c) = favorite_color {
        println!("using your favorite color: {}", c);
    } else if is_friday {
        println!("using friday color: red");
    } else if let Ok(a) = age {
        println!("your age {} colour: blue", a);
    } else {
        println!("default color: black");
    }

    // while let 条件循环
    // 与if let类似，当匹配不上时，结束循环
    let v = vec![1, 2, 3, 4, 5, 6, 7, 8, 9];
    let mut i = v.iter();
    while let Some(elem) = i.next() {
        // 123456789
        print!("{}", elem)
    }
    println!();
    let mut i = v.iter();
    // 任何能用于match的分支都能用于其它的匹配模式
    while let Some(1..=5) = i.next() {
        // aaaaa
        print!("a")
    }
    println!();

    // for循环
    // for循环自动把Some给解构了，并且自动调用了迭代器的next函数
    for (i, v) in v.iter().enumerate() {
        print!("({},{})", i, v);
    }
    println!();
    // 以上与以下等价
    let mut it = v.iter().enumerate();
    while let Some((i, v)) = it.next() {
        print!("({},{})", i, v);
    }
    println!();

    // let语句
    // 我们日常使用的let语句也是使用了模式匹配
    let (x, y, z) = (1, 2, 3);
    println!("x:{}, y:{}, z{}", x, y, z);

    // 函数参数也使用了模式匹配
    // 如下例子的函数参数类似于let &(x,y) = &p
    fn print_point(&(x, y): &(i32, i32)) {
        println!("point({},{})", x, y);
    }
    let p = (3, 4);
    print_point(&p); // point(3,4)

    // 可失败模式匹配和不可失败模式匹配
    // let, for, 函数参数必须是不可失败的模式匹配，在不可失败的模式匹配中使用可失败的模式，将发生编译报错
    // let只接收不可失败的匹配，所以下将编译报错
    // refutable pattern in local binding: `None` not covered
    //  `let` bindings require an "irrefutable pattern", like a `struct` or an `enum` with only one variant
    // let Some(x) = v.iter().next();

    // 在接收可失败的模式中使用不可失败模式，将收到编译器警告
    // irrefutable `if let` pattern
    // this pattern will always match, so the `if let` is useless
    if let x = 5 {
        println!("no meaning:{}", x)
    }
    let p = Point { x: 10, y: 10 };
    if let Point { x, y } = p {
        println!("pppp({}, {})", x, y);
    }

    // 使用解构来分解值
    // 可以使用模式来分解结构体、枚举、元组、引用
    // 解构结构体
    struct Point {
        x: i32,
        y: i32,
    }
    let p = Point { x: 10, y: 2 };
    // 指定字段值赋于的变量名a和b
    let Point { x: a, y: b } = p;
    println!("a:{}, b:{}", a, b); // a:10, b:2
    // 如果字段名与变量名一致时，可以简写成如下
    let Point { x, y } = p;
    println!("x:{}, x:{}", x, y); // x:10, x:2
    // 也可以使用match让某个字段指定某个值来匹配
    match p {
        Point { x: 0, y } => { println!("point at y axial, y:{}", y) }
        Point { x, y: 0 } => { println!("point at x axial, x:{}", x) }
        Point { x, y } => { println!("point at space:({}, {})", x, y) }
    }
    // 解构枚举之前看到的Option已经使用过多次了
    // 这里看一下解构嵌套的结构体和枚举
    let p = Some(Point { x: 10, y: 20 });
    if let Some(Point { x, y }) = p {
        println!("x:{}, x:{}", x, y); // x:10, x:20
    }
    // 解析结构体和元组
    let ((feat, inches), Point { x, y }) = ((3, 10), Point { x: 3, y: -10 });
    println!("{},{},{},{}", feat, inches, x, y); // 3,10,3,-10

    // 使用_来忽略某些值
    // 忽略第一个参数
    fn foo(_: i32, y: i32) {
        println!("{}", y);
    }
    // 忽略元组中的第二个元素
    let (x, _, z) = (1, 2, 3);
    println!("x:{}, z:{}", x, z);
    // 使用下划线开头的变量来防止编译器未使用的报警
    // 以下如果不使用_，则会有警告：unused variable: `x`
    // 注意，你还是可以使用下划线开头的变量的 println!("{}", _x);
    let _x = 10;
    // 下划线开头的变量依然会绑定值，这与纯下划线忽略值不一样
    let s = Some("hello".to_string());
    // 这里的_如果替换成_k，则最后打印s将报错，因为此时s中的值的所有权已经移到_k中了
    // 而_不会绑定值，所以还可以使用s
    if let Some(_) = s {}
    println!("{:?}", s);

    // 使用..来忽略值的值的剩余部分
    // 只匹配x的值，忽略剩下部分的字段
    let Point { x: _x, .. } = Point { x: 10, y: 10 };
    // 匹配第一个和最后一个，分别是1和9
    let (_first, .., _last) = (1, 2, 3, 4, 5, 6, 7, 8, 9);
    // 如果编译器无法确认匹配，即..产生歧义的匹配时，将报错
    //  can only be used once per tuple pattern
    // let (.., middle, ..) = (1,2,3,4,5,6,7,8,9);

    // 使用匹配守卫额外添加条件
    let c = Some(10);
    match c {
        // 在匹配分支后面添加if表达来额外限定匹配条件
        Some(x) if x > 6 => {}
        Some(_) => {}
        None => {}
    }

    // 使用@绑定
    // 用于像范围匹配这种条件的情况下，将匹配到的值赋值到变量
    let c = Some(2);
    if let Some(i @ 1..=5) = c {
        println!("c is between 1~5:{}", i);
    }
}