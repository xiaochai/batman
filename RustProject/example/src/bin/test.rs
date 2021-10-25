#[derive(Debug)]
struct E {
    a: String,
}

impl Drop for E {
    fn drop(&mut self) {
        println!("destroyed struct E");
    }
}

fn fn_once<F>(func: F) where F: FnOnce() {
    println!("fn_once begins");
    func();
    println!("fn_once ended");
}

fn main() {
    let e = E { a: "fn_once".to_string() };
    // 这样加个move，看看程序执行输出顺序有什么不同
    let f = move || println!("fn once calls: {:?}", e);
    // let f = || println!("fn once closure calls: {:?}", e);
    // fn_once(f);
    f();
    f();
    println!("main ended");
}