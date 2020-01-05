package mythread;

import java.util.concurrent.DelayQueue;
import java.util.concurrent.Delayed;
import java.util.concurrent.TimeUnit;

public class MyDelayQueue {
    public static void main(String[] args){
        DelayQueue<MyDelayed> delayQueues = new DelayQueue<>();
        for (int i = 0; i< 10; i++){
            delayQueues.put(new MyDelayed( i* 1000));
        }
        for(int i = 0; i< 10; i++){
            try {
                MyDelayed myDelayed = delayQueues.take();
                System.out.println(myDelayed);
            }catch (InterruptedException e){
                e.printStackTrace();
            }
        }
        // id : 0, delay : 0
        // id : 1, delay : 1000
        // id : 2, delay : 2000
        // id : 3, delay : 3000
        // id : 4, delay : 4000
        // id : 5, delay : 5000
        // id : 6, delay : 6000
        // id : 7, delay : 7000
        // id : 8, delay : 8000
        // id : 9, delay : 9000
    }

    static class MyDelayed implements Delayed {
        private long triggerTime; // in milliseconds
        private long delay; // in milliseconds
        private static int counter;
        private int id = counter++;

        public MyDelayed(long delay) {
            this.delay = delay;
            this.triggerTime = delay + System.nanoTime() / 1000000L;
        }

        @Override
        public long getDelay(TimeUnit unit) {
            return unit.convert(triggerTime - System.nanoTime()/1000000L, TimeUnit.MILLISECONDS);
        }

        @Override
        public int compareTo(Delayed o) {
//            return ((MyDelayed)o).id - this.id;
            long delta = this.getDelay(TimeUnit.MILLISECONDS) - o.getDelay(TimeUnit.MILLISECONDS) ;
            return delta == 0 ? 0 : (delta > 0 ? 1 : -1);
        }

        @Override
        public String toString(){
            return String.format("id : %d, delay : %d", id, delay);
        }
    }
}
