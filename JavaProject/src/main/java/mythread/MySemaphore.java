package mythread;

import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;

public class MySemaphore {
    public static void main(String[] args){
        Semaphore semaphore = new Semaphore(2);
        new Thread(()->{
            for(;;){
                try{
                    semaphore.acquire();
                    System.out.println("acquire");
                    TimeUnit.MILLISECONDS.sleep(1000);
                }catch (InterruptedException e){
                    e.printStackTrace();
                }
            }
        }).start();

        new Thread(()->{
            for(;;){
                try{
                    TimeUnit.MILLISECONDS.sleep(2000);
                    semaphore.release();
                    System.out.println("release");
                }catch (InterruptedException e){
                    e.printStackTrace();
                }
            }
        }).start();
    }
}
