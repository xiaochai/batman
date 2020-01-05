package mythread;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class MyLock {
    private int counter = 0;
    private Lock lock = new ReentrantLock();
    public void incr(){
        lock.lock();
        counter++;
        lock.unlock();
    }
    public int get(){
        return counter;
    }

    public static void main(String[] args) throws InterruptedException{
        ExecutorService threadService = Executors.newFixedThreadPool(10);
        MyLock myLock = new MyLock();
        for(int i = 0; i< 1000; i++){
            threadService.submit(()->myLock.incr());
        }
        TimeUnit.SECONDS.sleep(2);
        System.out.println(myLock.get());
        threadService.shutdown();
    }
}
