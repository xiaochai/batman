package mythread;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class MyAtomic {
    private AtomicInteger counter;
    public void incr(){
        counter.addAndGet(1);
    }
    public int get(){
        return counter.get();
    }

    private AtomicBoolean lock;

    public void lock(){
        while (lock.compareAndSet(false, true));
    }
    public void unlock(){
        lock.compareAndSet(true, false);
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
