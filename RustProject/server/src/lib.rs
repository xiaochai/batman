use std::sync::mpsc::{Receiver, Sender};
use std::thread;
use std::sync;
use std::sync::Arc;

pub struct ThreadPool {
    sender: Sender<Box<dyn FnOnce()+Send>>,
    threads: Vec<thread::JoinHandle<()>>
}

impl ThreadPool {
    pub fn new(s: usize) -> ThreadPool {
        let(sender, receiver) = std::sync::mpsc::channel();

        let mut threads = vec![];

        let receiver = sync::Mutex::new(receiver);
        let arc = Arc::new(receiver);

        for _ in 0..s{
            let new_arc = arc.clone();
            let h = thread::spawn(move||{
                loop {
                    let f:Box<dyn FnOnce()+Send> = new_arc.lock().unwrap().recv().unwrap();
                    f();
                }
            });
            threads.push(h);
        }

        ThreadPool{
            sender,
            threads
        }
    }
    pub fn execute<T>(&self, f: T) where T: FnOnce()+Send+'static {
        self.sender.send(Box::new(f));
    }
}
