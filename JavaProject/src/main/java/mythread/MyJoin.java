package mythread;

import java.util.concurrent.TimeUnit;

public class MyJoin extends Thread {
    private Thread t;

    public MyJoin(String name, Thread t) {
        super(name);
        this.t = t;
        this.start();
    }

    @Override
    public void run() {
        try {
            t.wait();
            if(t != null){
                t.join();
            }
            System.out.println(currentThread().getName() + ":start sleep");
            TimeUnit.SECONDS.sleep(2);
        } catch (InterruptedException e) {
            System.out.println(currentThread().getName() + ":" + e.getMessage());
        }
        System.out.println(currentThread().getName() + ":sleep done");
    }


    public static void main(String[] args) throws InterruptedException {
        MyJoin myJoin1 = new MyJoin("thread 1", null);
        MyJoin myJoin2 = new MyJoin("thread 2", myJoin1);
        // 结果
        // thread 1:start sleep
        // thread 1:sleep done
        // thread 2:start sleep
        // thread 2:sleep done
    }
}
