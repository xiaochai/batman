import proxy.Bird;
import proxy.IBird;
import proxy.ProxyFactory;
import org.junit.Test;

public class ProxyTest {
    @Test
    public void DyProxy(){
        IBird b = new Bird();
        ProxyFactory proxyFactory = new ProxyFactory(b);
        b = (IBird)proxyFactory.getProxyInstance();
        b.fly();
    }
}
