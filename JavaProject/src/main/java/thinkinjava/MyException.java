package thinkinjava;

public class MyException extends Exception {
    public static void main(String[] args) throws MyException{
        System.out.println(lostException());
        try {
            MyException.f();
        }catch (MyException e){
            e.initCause(new NullPointerException());
            throw e;
        }
    }
    public static void f() throws MyException{
        throw new MyException();
    }
    public static Object lostException(){
        try{
            throw new MyException();
        }finally {
            return 1;
        }
    }
}
/**
 Exception in thread "main" thinkinjava.MyException
    at thinkinjava.MyException.f(MyException.java:13)
    at thinkinjava.MyException.main(MyException.java:6)
 Caused by: java.lang.NullPointerException
    at thinkinjava.MyException.main(MyException.java:8)
 */