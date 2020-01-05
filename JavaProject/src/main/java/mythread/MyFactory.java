package mythread;


import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;

public class MyFactory implements ThreadFactory {
    @Override
    public Thread newThread(Runnable r) {
        Thread t = new Thread(r);
        t.setDaemon(true);
        t.setName("thread-my");
        return t;
    }
    public static void main(String[] args) throws InterruptedException{
        ExecutorService threadService = Executors.newCachedThreadPool(new MyFactory());
        threadService.submit(() -> System.out.println(Thread.currentThread().getName()));
        TimeUnit.SECONDS.sleep(1);
        // 输出为thread-my，并且在没有调用shutdown的情况下，主线程退出了，说明所创建的线程都为daemon线程


        Thread t = new Thread(()->{
            new Thread(()->System.out.println(Thread.currentThread().isDaemon())).start();
        });
        t.setDaemon(true);
        t.start();
    }
}
