package DesignPatterns;

import DesignPatterns.Proxy.Fly;
import DesignPatterns.Proxy.FlyProxy;
import org.junit.Test;

public class ProxyTest {
    @Test
    public void test(){
        Fly fly = new FlyProxy(new Fly());
        fly.dofly();
    }
}
