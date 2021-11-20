use std::io::{Read, Write};
use std::net::TcpListener;
use std::sync::mpsc;
use std::thread;
use std::time::Duration;

use signal_hook::consts::SIGINT;
use signal_hook::iterator::Signals;

use server::ThreadPool;

fn main() {
    // 起一个线程数量为4的线程池
    let thread_pool = ThreadPool::new(2);

    // 起一个线程单独处理信号，收到信号时，需要将信息传递给accept来结束接收连接，所以这里加了一个channel
    // 信号处理使用signal_hook包来处理
    let (terminal_sender, terminal_receiver) = mpsc::channel();
    let sig = thread::spawn(move || {
        let mut signal = Signals::new(&[SIGINT]).unwrap();
        for sig in signal.forever() {
            println!("rec sig:{}", sig);
            terminal_sender.send(()).unwrap();
            break;
        }
    });

    // 将listener放在单独的作用域里，这样当信号发出时，第一时间结束监听端口
    // 这样新的连接就不会打进来了，但旧的请求还是能被正常处理
    {
        // 使用TcpListener::bind让程序监听端口；
        // bind接收泛型参数，只要实现了ToSocketAddrs即可，例如String类型，不管是&str还是String都行
        let listener = TcpListener::bind("localhost:8888".to_string()).unwrap();

        // TcpListener的incoming返回迭代器，产生std::io::Result<TcpStream>类型
        // 也可以使用accept函数，将其放在loop中，其返回Result<(TcpStream, SocketAddr)>类型
        for stream in listener.incoming() {
            // 1. 最原始每一个请求阻塞处理
            // handler_connection(stream.unwrap());
            // 2. 每一个请求都启动一个线程来执行，但这样很快就会耗尽机器资源，容易被ddos攻击
            // thread::spawn(move || {
            //     handler_connection(stream.unwrap());
            // });
            // 3. 使用线程池来处理只保留有限的线程数来处理
            thread_pool.execute(move || {
                handler_connection(stream.unwrap());
            });
            // 由于把这个接收函数放在了接收任务时，所以这块有一个限制就是如果没有请求就无法运行到此处
            // 暂时没有想到很好的办法来处理这个
            if let Ok(_) = terminal_receiver.try_recv() {
                println!("get terminal sig!");
                break;
            }
        }
    }
    sig.join().unwrap();
    // 退出此作用域时，会调用thread_pool的drop函数来处理完请求
    println!("main end!");
}

// 处理每一个请求
fn handler_connection(mut stream: std::net::TcpStream) {
    let mut buffer = [0; 1000];
    let len = stream.read(&mut buffer).unwrap();
    let content = String::from_utf8_lossy(&buffer[0..10]);
    println!("receive a connection: remoteAddr {:?}, receive data len:{}, content:{:?}", stream.peer_addr().unwrap(), len, content);
    let index = b"GET / HTTP/1.1\r\n";
    let sleep = b"GET /sleep HTTP/1.1\r\n";
    if buffer.starts_with(index) {
        let response = "HTTP/1.1 200 OK\r\n\r\nOK";
        stream.write(response.as_bytes()).unwrap();
    } else if buffer.starts_with(sleep) {
        thread::sleep(Duration::from_secs(10));
        stream.write(b"HTTP/1.1 200 OK\r\n\r\nsleep").unwrap();
    } else {
        stream.write(b"HTTP/1.1 404 NOT FOUND\r\n\r\nnot found").unwrap();
    }
}