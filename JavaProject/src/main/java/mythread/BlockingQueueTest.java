package mythread;

import java.util.Random;
import java.util.concurrent.*;
import java.util.logging.Logger;

public class BlockingQueueTest {
    public static void main(String[] args){
        test(new SynchronousQueue<>());
    }

    public static void test(BlockingQueue<Integer> queue){
        // 生产者
        new Thread(()->{
            while (true) {
                try {
                    Integer v = new Random().nextInt(100);
                    queue.put(v);
                    System.out.println("put a value:" + v);
                    TimeUnit.MILLISECONDS.sleep(100);
                }catch (InterruptedException e){
                    e.printStackTrace();
                    break;
                }
            }
        }).start();

        // 消费者
        while (true){
            try{
                Integer v = queue.take();
                System.out.println("take a value:" + v);
                TimeUnit.MILLISECONDS.sleep(500);
            }catch (InterruptedException e){
                e.printStackTrace();
                break;
            }
        }
    }
}
