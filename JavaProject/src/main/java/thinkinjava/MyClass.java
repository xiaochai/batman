package thinkinjava;

import java.lang.reflect.Method;

public class MyClass {
    public static void main(String[] args){
        A a = new C();
        System.out.println(a.getClass());
        System.out.println(a.getClass().isInterface());
        System.out.println(a instanceof C);
        System.out.println(C.class.isInstance(a));
        System.out.println(A.class.isAssignableFrom(C.class));

        System.out.println(boolean.class == Boolean.class); //false
        System.out.println(boolean.class == Boolean.TYPE); // true


    }
}
