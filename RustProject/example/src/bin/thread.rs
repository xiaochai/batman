fn main() {
    let delta = 10;

    // 使用thread::spawn启动一个线程，rust原生不支持协程，但有其它库提供支持
    // spawn的参数是一个无参数装饰或者函数，会在另外一个线程中运行这个函数的代码
    // 返回一个Handler，可以调用其join函数等待线程执行完成
    // 如果在闭包中使用环境中的值，需要将其所有权move到新线程中
    let t = std::thread::spawn(move || {
        for i in 1..10 {
            println!("in thread {}", i + delta);
            std::thread::sleep(std::time::Duration::from_millis(1));
        }
    });
    for i in 1..5 {
        println!("out thread {}", i+delta);
        std::thread::sleep(std::time::Duration::from_millis(1));
    }
    t.join().unwrap()
}