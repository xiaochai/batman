use minigrep::*;

// main函数里的代码无法进行单元测试和集成测试，所以main函数这块保持简单，把逻辑移到lib.rs下
fn main() {
    // std::env::args()返回的是Args实现了Iterator，使用collect转化为Vec<String>
    // 因为collect返回的也是泛型，编译器无法自动推断需要返回的类型，所以给args的类型标注不可省略
    let args: Vec<String> = std::env::args().collect();

    // Result结构的unwrap_or_else方法接收一个函数来做错误处理
    // 处理函数包含一个闭包参数，使用竖线包起来，获取到的值为Result的E中的值
    // 处理函数必须返回Result的T值(正确的值)，但std::process::exit(1)直接退出进程，所以可以编译通过
    // exit的返回值为->!，表示从不会返回：pub fn exit(code: i32) -> !
    let config = Config::new(&args).unwrap_or_else(|err| {
        // 使用eprintln!将错误信息输出到标准错误输出
        eprintln!("Problem parsing arguments: {}", err);
        std::process::exit(1);
    });

    // // 使用迭代器的版本
    // let config = Config::new2(std::env::args()).unwrap_or_else(|err| {
    //     eprintln!("Problem parsing arguments: {}", err);
    //     std::process::exit(1);
    // });

    // 使用if let语法，只处理关心的变体
    if let Err(e) = run(&config) {
        eprintln!("Application error: {}", e);
        std::process::exit(1);
    }
}
