package mythread;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

public class MyInterrupt extends Thread {
    private int duration;
    public MyInterrupt(String name, int duration) {
        super(name);
        this.duration = duration;
        this.start();
    }

    @Override
    public void run() {
        try{
            System.out.println(currentThread().getName() + ":start sleep");
            TimeUnit.SECONDS.sleep(duration);
        }catch (InterruptedException e){
            System.out.println(currentThread().getName() + ":" + e.getMessage());
            System.out.println(currentThread().getName() + " is interrepted:" + currentThread().isInterrupted());
        }
    }


    public static void main(String[] args) throws InterruptedException{
        MyInterrupt myInterrupt = new MyInterrupt("myjoin", 10);
        TimeUnit.SECONDS.sleep(1);
        myInterrupt.interrupt();
        System.out.println("thread isInterrupt:" + myInterrupt.isInterrupted());
        TimeUnit.SECONDS.sleep(1);
        // 返回结果
        // myjoin:start sleep
        // thread isInterrupt:true
        // myjoin:sleep interrupted
        // myjoin is interrepted:false

        ExecutorService executorService = Executors.newCachedThreadPool();
        Future<?> f = executorService.submit(
                () -> {
                    while (true){
                        try{
                            TimeUnit.SECONDS.sleep(10);
                        }catch (InterruptedException e){
                            System.out.println("get a interrupted exception:" + e.getMessage());
                            break;
                        }
                    }
                    System.out.println("task stop!");
                }
        );
        TimeUnit.SECONDS.sleep(1);
        f.cancel(true);
        TimeUnit.SECONDS.sleep(1);
        executorService.shutdown();
        // get a interrupted exception:sleep interrupted
        // task stop!

    }
}
