package Proxy;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

public class ProxyFactory {
    private Object target;

    public ProxyFactory(Object o) {
        this.target = o;
    }
    public Object getProxyInstance(){
        return Proxy.newProxyInstance(target.getClass().getClassLoader(),
                target.getClass().getInterfaces(),
                (Object proxy, Method method, Object[] args)->{
                    System.out.println("before");
                    method.invoke(target, args);
                    System.out.println("after");
                    return null;
                }
        );
    }
}
