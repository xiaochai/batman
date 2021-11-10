use std::rc::Rc;

fn main() {
    let v = vec![1, 2, 3];

    // 使用thread::spawn启动一个线程，rust原生不支持协程，但有其它库提供支持
    // spawn的参数是一个无参数装饰或者函数，会在另外一个线程中运行这个函数的代码
    // 返回一个Handler，可以调用其join函数等待线程执行完成
    // 如果在闭包中使用环境中的值，需要将其所有权move到新线程中
    // 否则报错closure may outlive the current function, but it borrows `v`, which is owned by the current function
    let t = std::thread::spawn(move || {
        for i in 1..10 {
            println!("in thread {}", i + v[0]);
            std::thread::sleep(std::time::Duration::from_millis(1));
        }
    });
    for i in 1..5 {
        println!("out thread {}", i);
        std::thread::sleep(std::time::Duration::from_millis(1));
    }
    // 由于v的所有权已经被移入新线程了，所以这里就无法再使用v了
    // println!("v:{:?}", v);

    // 使用JoinHandler的join方法来等待线程执行完成
    // 返回的是Result<T, E>类型
    t.join().unwrap();


    // 使用通道(channel)在线程中传递信息
    // mpsc是multiple producer,single consumer的缩写，顾名思义，只能有一个消费者，但可以多个发送方
    let (tx, rx) = std::sync::mpsc::channel();

    std::thread::spawn(move || {
        let hi = "hi".to_string();
        tx.send(hi).unwrap();
        // send会将取得没有实现Copy trait的所有权，所以以下语句将报错
        // println!("send {}", hi);
    });

    // recv会阻塞线程，直到收到消息
    // 可以使用try_recv来非阻塞地试探是否有消息可以接收，没有消息时将返回Err
    println!("{}", rx.recv().unwrap());

    let (tx, rx) = std::sync::mpsc::channel();
    // mpsc支持多个发送方，可以通过clone方法来创建出多个发送端来
    let tx_2 = std::sync::mpsc::Sender::clone(&tx);
    std::thread::spawn(move || {
        for _i in 1..5 {
            tx.send("hi".to_string());
            std::thread::sleep(std::time::Duration::from_millis(100))
        }
    });
    std::thread::spawn(move || {
        for _i in 1..10 {
            tx_2.send("there".to_string());
            std::thread::sleep(std::time::Duration::from_millis(100))
        }
    });

    // 将rx当成迭代器来遍历收到的数据
    // 由于tx在线程退出时执行了Drop，整个通道就关闭了，所以rx的迭代就会结束了
    for m in rx {
        println!("got:{}", m)
    }

    // mutex的使用
    // 使用new函数创建互斥量，存储一个i32的值
    let m = std::sync::Mutex::new(10);
    {
        // 使用lock返回一个Result，正常时为MutexGuard是一个智能指针
        let mut i = m.lock().unwrap();
        // 修改值为20
        *i = 20;
        // 在这个作用域的结尾，智能指针执行了drop方法，将m解锁了
    }
    // m现在的值为20了
    // Mutex { data: 20, poisoned: false, .. }
    println!("{:?}", m);

    // 并发场景下使用mutex
    use std::sync::{Mutex, Arc};
    use std::thread;
    // Arc<T>是并发场景下的Rc<T>
    // 使用Rc<T>会报错：`Rc<Mutex<i32>>` cannot be sent between threads safely
    // the trait `Send` is not implemented for `Rc<Mutex<i32>>`
    let counter = Arc::new(Mutex::new(0));
    // let countet1 = Rc::new(Mutex::new(0));
    let mut handlers = vec![];
    for i in 0..30 {
        let c = counter.clone();
        // let c2 = countet1.clone();
        let h = thread::spawn(move || {
            let mut t = c.lock().unwrap();
            // c2.lock().unwrap();
            *t += 1;
        });
        handlers.push(h);
    }
    for h in handlers {
        h.join().unwrap();
    }
    println!("{:?}", counter)
}