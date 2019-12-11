package thinkinjava;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public class MyReflect {
    public static void main(String[] args) {
        for (Method m : P.class.getMethods()) {
            System.out.println(m);
        }
        System.out.println("============");
        for (Method m : P.class.getDeclaredMethods()) {
            System.out.println(m);
        }

        try {
            Method m = P.class.getDeclaredMethod("f");
            m.invoke(new P());
        }catch (NoSuchMethodException e){
            System.out.println("no such method!");
        }catch (IllegalAccessException |InvocationTargetException e){
            System.out.println(e.toString());
        }
        try {
            Method m = P.class.getDeclaredMethod("f");
            m.setAccessible(true);
            m.invoke(new P());
        }catch (NoSuchMethodException e){
            System.out.println("no such method!");
        }catch (IllegalAccessException |InvocationTargetException e){
            System.out.println(e.toString());
        }

    }
}

class P {
    private void f(){
        System.out.println("private method f call");
    }
}
/**
 public final void java.lang.Object.wait(long,int) throws java.lang.InterruptedException
 public final native void java.lang.Object.wait(long) throws java.lang.InterruptedException
 public final void java.lang.Object.wait() throws java.lang.InterruptedException
 public boolean java.lang.Object.equals(java.lang.Object)
 public java.lang.String java.lang.Object.toString()
 public native int java.lang.Object.hashCode()
 public final native java.lang.Class java.lang.Object.getClass()
 public final native void java.lang.Object.notify()
 public final native void java.lang.Object.notifyAll()
 ============
 private void thinkinjava.P.f()
 java.lang.IllegalAccessException: Class thinkinjava.MyReflect can not access a member of class thinkinjava.P with modifiers "private"
 private method f call
 */