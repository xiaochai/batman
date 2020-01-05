package mythread;

import java.util.Comparator;
import java.util.concurrent.PriorityBlockingQueue;

public class MyPriorityBlockingQueue {
    public static void main(String[] args){
        PriorityBlockingQueue<MyItem> queue = new PriorityBlockingQueue<>();
        for(int i = 0; i< 10; i++){
            queue.add(new MyItem());
        }
        while (true) {
            try {
                System.out.println(queue.take());
            }catch (InterruptedException e){
                e.printStackTrace();
            }
        }
        // id: 0
        // id: 1
        // id: 2
        // id: 3
        // id: 4
        // id: 5
        // id: 6
        // id: 7
        // id: 8
        // id: 9
    }
    static class MyItem implements Comparable<MyItem> {
        private static int counter;
        private int id = counter++;
        @Override
        public int compareTo(MyItem o) {
            return id - o.id;
        }
        @Override
        public String toString(){
            return "id: " + id;
        }
    }
}

class MyItem2{
    private static int counter;
    private int id = counter++;
    public int getId(){
        return id;
    }
    @Override
    public String toString(){
        return "id: " + id;
    }
    public static void main(String[] args) {
        PriorityBlockingQueue<MyItem2> queue = new PriorityBlockingQueue<>(10, new MyItem2Comparator());
        for (int i = 0; i < 10; i++) {
            queue.add(new MyItem2());
        }
        while (true) {
            try {
                System.out.println(queue.take());
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }
}

class MyItem2Comparator implements Comparator<MyItem2>{
    @Override
    public int compare(MyItem2 o1, MyItem2 o2) {
        return o1.getId() - o2.getId();
    }
}
