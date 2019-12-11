package proxy;

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
                    System.out.println("before:" + method.getName());
                    method.invoke(proxy, args);
                    System.out.println("after:" + method.getName());
                    return null;
                }
        );
    }

    public static void main(String[] args){
        // 可将生成的class文件保存下来，这样可以观察生成的代码
        // 生成目录com/sum/proxy/$Proxy0.class
        System.getProperties().put("sun.misc.ProxyGenerator.saveGeneratedFiles", "true");

        ProxyFactory proxyFactory = new ProxyFactory(new Bird());
        IBird b = (IBird) proxyFactory.getProxyInstance();
        b.fly2();
    }
}
