package mythread;

import java.util.concurrent.BrokenBarrierException;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.TimeUnit;

public class MyCylicBarrier {
    public static void main(String[] args){
        int size = 10;
        CyclicBarrier barrier = new CyclicBarrier(size, ()->{
           System.out.println("barrier action done");
        });

        for(int i = 0; i< size; i++){
            new Thread(()->{
                try{
                    TimeUnit.SECONDS.sleep(1);
                    System.out.println("task done");
                    barrier.await();
                    System.out.println("task await done");
                }catch (InterruptedException e){
                    e.printStackTrace();
                }catch (BrokenBarrierException e){
                    e.printStackTrace();
                }
            }).start();
        }
        // task done
        // task done
        // task done
        // task done
        // task done
        // task done
        // task done
        // task done
        // task done
        // task done
        // barrier action done
        // task await done
        // task await done
        // task await done
        // task await done
        // task await done
        // task await done
        // task await done
        // task await done
        // task await done
        // task await done
    }
}
