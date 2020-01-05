package mythread;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class ConditionQueue<T> {
    private T tmp = null;
    private Lock lock = new ReentrantLock();
    private Condition condition = lock.newCondition();
    public T get(){
        lock.lock();
        try {
            while (tmp == null) {
                try {
                    condition.await();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
            T t = tmp;
            tmp = null;
            return t;
        }finally {
            lock.unlock();
        }
    }

    public void set(T t){
        lock.lock();
        try {
            tmp = t;
            condition.signal();
        }finally {
            lock.unlock();
        }
    }

    public static void main(String[] args) throws InterruptedException{
        ConditionQueue<Integer> notifyQueue = new ConditionQueue<>();
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
