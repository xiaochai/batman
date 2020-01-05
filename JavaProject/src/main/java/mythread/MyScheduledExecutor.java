package mythread;

import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

public class MyScheduledExecutor {
    public static void main(String[] args){
        ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(2);
        executor.schedule(()->System.out.println("schedule !!"), 1, TimeUnit.SECONDS);
        executor.scheduleAtFixedRate(()->System.out.println("scheduleAtFixedRate"), 2, 1, TimeUnit.SECONDS);
        executor.scheduleWithFixedDelay(()->System.out.println("scheduleWithFixedDelay"), 3, 1,  TimeUnit.SECONDS);
    }
}
