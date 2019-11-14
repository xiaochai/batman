package designpatterns;

import designpatterns.prototype.SumPool;
import org.junit.Test;

public class PrototypeTest {
    @Test
    public void test() {
        SumPool sumPool = new SumPool();
        System.out.printf("%d,%d,%d,%d",
                sumPool.getSum(100).getTotal(),
                sumPool.getSum(10000).getTotal(),
                sumPool.getSum(100).getTotal(),
                sumPool.getSum(10000).getTotal());
    }
}
