use std::io::{Read, Write};
use std::net::TcpListener;
use std::thread;
use std::time::Duration;
use server::ThreadPool;

fn main() {
    // 使用TcpListener::bind让程序监听端口；
    // bind接收泛型参数，只要实现了ToSocketAddrs即可，例如String类型，不管是&str还是String都行
    let listener = TcpListener::bind("localhost:8888".to_string()).unwrap();

    let thread_pool = ThreadPool::new(2);


    // TcpListener的incoming返回迭代器，产生std::io::Result<TcpStream>类型
    // 也可以使用accept函数，将其放在loop中，其返回Result<(TcpStream, SocketAddr)>类型
    for stream in listener.incoming() {
        // 1. 最原始每一个请求阻塞处理
        // handler_connection(stream.unwrap());
        // 2. 每一个请求都启动一个线程来执行，但这样很快就会耗尽机器资源，容易被ddos攻击
        // thread::spawn(move || {
        //     handler_connection(stream.unwrap());
        // });

        thread_pool.execute(move ||{
            handler_connection(stream.unwrap());
        });

    }
}

// 处理每一个请求
fn handler_connection(mut stream: std::net::TcpStream) {
    let mut buffer = [0; 1000];
    let len = stream.read(&mut buffer).unwrap();
    let content = String::from_utf8_lossy(&buffer[0..30]);
    println!("receive a connection: remoteAddr {:?}, receive data len:{}, content:{:?}", stream.peer_addr().unwrap(), len, content);
    let index = b"GET / HTTP/1.1\r\n";
    let sleep = b"GET /sleep HTTP/1.1\r\n";
    if buffer.starts_with(index) {
        let response = "HTTP/1.1 200 OK\r\n\r\nOK";
        stream.write(response.as_bytes()).unwrap();
    } else if buffer.starts_with(sleep) {
        thread::sleep(Duration::from_secs(5));
        stream.write(b"HTTP/1.1 200 OK\r\n\r\nsleep").unwrap();
    } else {
        stream.write(b"HTTP/1.1 404 NOT FOUND\r\n\r\nnot found").unwrap();
    }
}