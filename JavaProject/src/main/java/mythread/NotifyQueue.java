package mythread;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class NotifyQueue<T> {
    T tmp = null;
    public synchronized T get(){
        while (tmp == null){
            try {
                wait();
            }catch (InterruptedException e){
                e.printStackTrace();
            }
        }
        T t = tmp;
        tmp = null;
        return t;
    }

    public synchronized void set(T t){
        tmp = t;
        notify();
    }

    public static void main(String[] args) throws InterruptedException{
        NotifyQueue<Integer> notifyQueue = new NotifyQueue<>();
        ExecutorService executorService = Executors.newCachedThreadPool();
        for(int i = 0; i< 5; i++){
            executorService.submit(()->{
                while(true){
                    System.out.println(Thread.currentThread().getName() + " get:" + notifyQueue.get());
                }
            });
        }

        while(true){
            System.out.println("set value");
            notifyQueue.set(10);
            TimeUnit.SECONDS.sleep(1);
        }
        // pool-1-thread-4 get:10
        // set value
        // pool-1-thread-5 get:10
        // set value
        // pool-1-thread-2 get:10
        // set value
        // pool-1-thread-1 get:10
        // set value
        // pool-1-thread-3 get:10
        // set value
        // pool-1-thread-4 get:10
    }
}
