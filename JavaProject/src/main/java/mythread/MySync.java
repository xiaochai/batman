package mythread;


import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class MySync {
    private int counter = 0;

    public synchronized void incr() {
        counter++;
    }

    public int get() {
        return counter;
    }

    public static void main(String[] args) throws InterruptedException {
        ExecutorService threadService = Executors.newFixedThreadPool(10);
        MySync mySync = new MySync();
        for (int i = 0; i < 1000; i++) {
            threadService.submit(() -> mySync.incr());
        }
        TimeUnit.SECONDS.sleep(2);
        System.out.println(mySync.get());
        threadService.shutdown();
    }
}


class MySync1 {
    public synchronized void f() {
        System.out.println("f start");
        try {
            TimeUnit.SECONDS.sleep(5);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        System.out.println("f end");
    }

    public static void main(String[] args) {
        MySync1 mySync1 = new MySync1();
        new Thread(() -> mySync1.f()).start();
        synchronized (mySync1) {
            System.out.println("main in sync");
            try {
                TimeUnit.SECONDS.sleep(5);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            System.out.println("main end");
        }
        // 输出如下，说明synchronized同步控制块与方法上使用的对象锁为同一个
        // main in sync
        // main end
        // f start
        // f end
    }
}