package designpatterns;

import designpatterns.proxy.Fly;
import designpatterns.proxy.FlyProxy;
import org.junit.Test;

public class ProxyTest {
    @Test
    public void test(){
        Fly fly = new FlyProxy(new Fly());
        fly.dofly();
    }
}
