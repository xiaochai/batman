package mythread;

import java.util.concurrent.TimeUnit;

public class Simple {
    public static void main(String[] args){
        Thread t = new Thread(new Print());
        // setDaemon是用来设置线程为后台线程，默认线程都为非后台线程。
        // 只有当所有非后台线程都退出后，主线程才结束。
        // t.setDaemon(true);
        // 设置线程的级别，为了避免不同平台的差异，推荐使用MAX_PRIORITY、NORM_PIORITY、MIN_PRIORITY
        t.setPriority(Thread.MAX_PRIORITY);
        t.start();
        System.out.println("main");
    }

    static class Print implements Runnable{
        @Override
        public void run() {
            try {
                TimeUnit.SECONDS.sleep(5);
                // 或者使用以下语句可以达到同样的效果
                //Thread.sleep(5000);
            }catch (InterruptedException e){
                e.printStackTrace();
            }
            System.out.println("thread");
            // yield用于给调度暗示，表明当前线程已经完成重要工作，这时正是换出cpu的好时机。但这也只是给一个暗示，具体操作还和其它的因素有关。
            Thread.yield();
        }
    }
}


