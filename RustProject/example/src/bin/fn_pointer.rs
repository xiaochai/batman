fn main() {
    // 我们之前试过使用函数指针，闭包的Fn, FnMut, FnOnce这些trait来实现参数传递
    // 除此之外，还可以返回函数类型和trait类型做为函数的返回值
    // 以下函数，以函数指针做为返回值
    fn return_fn() -> fn(i32) -> i32 {
        |x| x + 100
    }
    let t = return_fn();
    println!("{}", t(10)); // 110

    // 以下函数，返回实现了Fn trait的函数指针或者是闭包
    fn return_fn_trait() -> Box<dyn Fn(i32) -> i32> {
        Box::new(|x|  x + 100 )
    }
    let t= return_fn_trait();
    println!("{}", t(10));
}