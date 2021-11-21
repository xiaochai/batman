fn main() {
    // 闭包当作匿名函数的一般写法，参数和返回值的类型，由编译器推断出来
    let add_one = |x| {
        x + 1
    };
    // 如果无法推断，则需要写成这种详细的形式，与函数定义非常类似
    let add_one2 = |x: i32| -> i32{
        x + 1
    };
    // 在匿名函数没有捕获环境变量的情况下，与函数一样
    fn add_one_fn(x: i32) -> i32 {
        x + 1
    }

    fn use_fn(x: i32, f: fn(i32) -> i32) -> i32 {
        f(x)
    }

    // 输出2,2,2
    // 可见无捕获上下文的闭包与函数一样
    println!("{},{},{}", use_fn(1, add_one), use_fn(1, add_one2), use_fn(1, add_one_fn));

    // 如果捕获了环境变量，则闭包与函数就不能通用了
    let k = 1;
    let add_one_closure = |x: i32| {
        x + k + 1
    };
    // 这里报错，expected fn pointer, found closure
    // closures can only be coerced to `fn` types if they do not capture any variables
    // println!("{}", use_fn(1, add_one_closure));

    // 函数无法捕获上下文:can't capture dynamic environment in a fn item
    //  use the `|| { ... }` closure form instead
    // fn f_error(x: i32) -> i32 {
    //     x + k + 1;
    // }

    // 以下正常运行，输出12
    println!("{}", add_one_closure(10));

    // 多个参数时，使用逗号隔离开
    let sum = |x, y| {
        x + y
    };
    // 如果只有一行，可以省略掉大括号
    let sum2 = |x, y| x + y;
    println!("{},{}", sum(1, 2), sum2(2, 3));

    // 在不指定类型的闭包，只能推断出一种类型
    let closure = |x| x;
    println!("{}", closure(10));
    // 以下将报错，办为之前的调用已经推断closure为(i32)->i32的闭包，不能再使用&str类型了
    // expected integer, found `&str`
    // println!("{}", closure("10"));

    // 与闭包相关的三种trait： Fn, FnMut, FnOnce，所有闭包至少实现其中的一个
    // 这三个trait代表了函数接收参数的三种方式：不可变引用(Fn)，可变引用(FnMut)，获取所有权(FnOnce)
    // rust自动从闭包的定义中推导出其所实现的trait
    // 所有函数都实现了这三个trait，所以要求传闭包的地方都可以传函数

    // 这三个trait的区别：
    // 实现了FnOnce的闭包可以从环境中获取变量的所有权，所以这个类型的闭包只能被调用一次
    // 实现了FnMut的闭包从环境中可变地借用值
    // 实现了Fn的闭包从环境中不可变地借用值
    // 所有闭包都实现了FnOnce，实现了Fn的闭包也实现了FnMut
    // 接收Fn trait做为参数的函数
    fn do_1<T: Fn(i32) -> i32>(x: i32, f: T) -> i32 {
        f(x)
    }
    let k = vec![1, 2];
    let c_1 = |x| x + k[0] + 1;
    fn f_1(x: i32) -> i32 { x + 1 }
    //
    // 输出5,3
    println!("{},{}", do_1(1, c_1), do_1(1, f_1));
    println!("{}", c_1(1));

    // 使用move关键可以获取变量的所有权
    let c_1 = move |x| x + k[0] + 1;
    // ？？？很奇怪的现象，如果加上下面这一行，则后面的两次调用将报错: borrow of moved value: `c_1`
    // 但如果没有下面这一行，则后面的两次调用则可以通过
    // do_1(1, c_1);
    println!("{}", c_1(1));
    println!("{}", c_1(1));


    // FnOnce的例子
    fn do_2<T: Fn() -> String>(f: T) {
        f();
    }
    let a = "aa".to_string();
    // c_2因为返回了a变量，而String没有实现Copy trait，所以相当于c_2获取了a的所有权并返回了a，所以c_2只实现了FnOnce trait，无法被传递到Fn作为参数的函数中
    let c_2 = || a;
    // 以下一行将报错： this closure implements `FnOnce`, not `Fn`
    // do_2(c_2);
    fn do_3<T: FnOnce() -> String>(f: T) {
        f();
    }
    // 运行正常
    do_3(c_2);
    // 以下编译报错： use of moved value: `c_2`
    // 因为c_2是FnOnce没有实现Copy，所以无法被使用两次
    // do_3(c_2);

    // FnMut的例子
    let mut k = 3;
    let mut c_3 = || {
        k = k + 1;
        k
    };
    println!("{}", c_3());
    // 以下将无法通过编译，因为c_3使用k的可变引用，不能再拥有k的其它引用了
    // println!("{},{}", k, c_3());
}
