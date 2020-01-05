package mythread;

import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.*;

public class MyExecutor implements Runnable {
    public static void main(String[] args) throws InterruptedException {
        ExecutorService service = Executors.newCachedThreadPool();
        service.submit(new MyExecutor());
        service.submit(new MyExecutor());
        TimeUnit.SECONDS.sleep(2);
        service.submit(new MyExecutor());
        service.submit(new MyExecutor());
        TimeUnit.SECONDS.sleep(2);
        service.shutdown();
        // 以上输出为10,11,11,10，说明后两执行复用了前两的线程

        ExecutorService serviceFix = Executors.newFixedThreadPool(2);
        for (int i = 0; i < 10; i++) {
            serviceFix.submit(new MyExecutor());
        }
        TimeUnit.SECONDS.sleep(2);
        serviceFix.shutdown();
        // 输出13,14,13,14,13,14,13,14,13,14，说明serviceFix只起了13、14两个线程来执行

        ExecutorService serviceSingle = Executors.newSingleThreadExecutor();
        for (int i = 0; i < 10; i++) {
            serviceSingle.submit(new MyExecutor());
        }
        TimeUnit.SECONDS.sleep(2);
        serviceSingle.shutdown();
        // 输出为15,15,15,15,15,15,15,15,15,15,SingleThreadExecutor是数量为1的FixedThreadPool

        ExecutorService serviceCallable = Executors.newCachedThreadPool();
        Future<Long> future = serviceCallable.submit(new MyCallable());
        try {
            System.out.print(future.isDone()+",");
            System.out.print(future.get()+",");
        }catch (ExecutionException e){
            e.printStackTrace();
        }
        serviceCallable.shutdown();
        serviceCallable.awaitTermination(2, TimeUnit.SECONDS);
        // 输出false,16,

    }

    final static class MyCallable implements Callable<Long>{
        @Override
        public Long call(){
            return Thread.currentThread().getId();
        }
    }

    @Override
    public void run() {
        System.out.print(Thread.currentThread().getId() + ",");
    }
}
