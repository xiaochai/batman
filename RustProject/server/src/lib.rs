use std::sync::{Arc, mpsc, Mutex};
use std::sync::mpsc::{Receiver, Sender};
use std::thread;

// 封闭的线程运行环境
pub struct Worker {
    id: usize,
    // 为什么要用Option，因为drop的时候，需要获取到thread的所有权
    // 使用take获取所有权之后，此处的值就是None了
    thread: Option<thread::JoinHandle<()>>,
}

impl Worker {
    fn new(id: usize, receiver: Arc<Mutex<Receiver<Message>>>) -> Worker {
        // 每一个worker会起一个线程来接收对应的消息
        let thread = thread::spawn(move || {
            loop {
                // receiver需要lock，这里在let语句结束之后，这个receive已经unlock了
                // 因为如果MutexGuard不再使用，则将drop掉
                let message = receiver.lock().unwrap().recv().unwrap();
                // 换成以下两句，则这个l走到job完成后才释放unlock，不满足要求
                // let l = receiver.lock().unwrap();
                // let message = l.recv().unwrap();
                match message {
                    Message::Terminal => {
                        break;
                    }
                    Message::NewJob(job) => {
                        print!("{} is doing the job：", id);
                        job();
                    }
                }
            }
        });
        let thread = Some(thread);
        Worker { id, thread }
    }
}

// 一个任务即一个闭包函数，因为需要用于在不同的线程之间传递，所以需要实现Send
pub type Job = Box<dyn FnOnce() + Send>;

// 用于发送任务用的Message，有两种类型
// 一种是Terminal结束线程运行，另外一种是NewJob消息，即新的任务
pub enum Message {
    Terminal,
    NewJob(Job),
}

// 线程池，包含有发送Message用的sender，以及运行的Worker
pub struct ThreadPool {
    sender: Sender<Message>,
    workers: Vec<Worker>,
}

impl ThreadPool {
    /// 创建一个指定线程数的线程池
    ///
    /// # Panics
    ///
    /// `new`函数会在参数为0时panic
    pub fn new(s: usize) -> ThreadPool {
        assert!(s > 0);
        // 使用mpsc::channel来与给多个线程传递信息，但由于不能有多个消费者(即Receiver没有实现Sync)
        let (sender, receiver) = mpsc::channel();
        // with_capacity可以用于预分配对应大小的动态数组，减少动态扩容
        let mut workers = Vec::with_capacity(s);

        // receiver没有实现Sync，需要使用Mutex包裹起来实现Sync，另外需要在多个线程中使用，必须使用Arc引用计数
        let receiver = Arc::new(Mutex::new(receiver));

        for id in 0..s {
            // 每一个线程就是一个worker，保存于workers中
            workers.push(Worker::new(id, receiver.clone()));
        }

        ThreadPool { sender, workers }
    }
    pub fn execute<T>(&self, f: T) where T: FnOnce() + Send + 'static {
        self.sender.send(Message::NewJob(Box::new(f))).unwrap();
    }
}

impl Drop for ThreadPool {
    fn drop(&mut self) {
        for _ in &mut self.workers {
            println!("send terminal to workers");
            self.sender.send(Message::Terminal).unwrap();
        }
        for w in &mut self.workers {
            // 因为join需要获取所有权，所以这块使用了take
            if let Some(t) = w.thread.take() {
                t.join().unwrap();
                println!("No. {} thread has terminal!", w.id);
            }
        }
    }
}
