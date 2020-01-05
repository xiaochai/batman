package mythread;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

public class MyCountDwonLatch {
    public static void main(String[] args){
        int size = 10;
        CountDownLatch countDownLatch = new CountDownLatch(size);
        for(int i = 0; i< size; i++){
            new Thread(()->{
                try {
                    TimeUnit.SECONDS.sleep(1);
                }catch (InterruptedException e){
                    e.printStackTrace();
                }

                System.out.print("job done，");
                countDownLatch.countDown();
            }).start();
        }
        try {
            countDownLatch.await();
        }catch (InterruptedException e){
            e.printStackTrace();
        }
        System.out.println("job all done");
        // job done，job done，job done，job done，job done，job done，job done，job done，job done，job done，job all done
    }
}
