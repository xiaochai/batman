package DesignPatterns.Prototype;

import java.util.HashMap;

public class SumPool {
    private HashMap<Integer, Sum> pool;

    public SumPool() {
        this.pool = new HashMap<>();
    }

    public Sum getSum(int i) {
        try {
            if (pool.get(i) != null) {
                return pool.get(i).clone();
            }
        }catch (CloneNotSupportedException e){
        }
        System.out.println("create new " + i);
        Sum sum = new Sum(i);
        pool.put(i, sum);
        return sum;
    }
}
