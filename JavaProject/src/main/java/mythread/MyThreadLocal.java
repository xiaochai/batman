package mythread;

public class MyThreadLocal {
    public static void main(String[] args) {
        for (int i = 0; i < 2; i++) {
            new Thread(() -> {
                MyThreadLocal.counter.set(1);
                for (int j = 0; j < 10; j++) {
                    System.out.println(Thread.currentThread().getName() + ":" + MyThreadLocal.counter.get());
                    Thread.yield();
                    MyThreadLocal.counter.set(MyThreadLocal.counter.get() + 1);
                }
            }).start();
        }
//        Thread-0:1
//        Thread-0:2
//        Thread-1:1
//        Thread-0:3
//        Thread-1:2
//        Thread-0:4
//        Thread-1:3
//        Thread-0:5
//        Thread-1:4
//        Thread-0:6
//        Thread-1:5
//        Thread-0:7
//        Thread-1:6
//        Thread-0:8
//        Thread-1:7
//        Thread-0:9
//        Thread-1:8
//        Thread-0:10
//        Thread-1:9
//        Thread-1:10
    }

    static ThreadLocal<Integer> counter = new ThreadLocal<>();
}
