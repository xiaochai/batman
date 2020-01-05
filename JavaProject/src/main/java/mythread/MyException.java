package mythread;

public class MyException {
    public static void main(String[] args){
        Thread t = new Thread(() -> {
            throw new RuntimeException();
        });
        try {
            t.start();
            t.join();
        }catch (Exception e){
            System.out.println("catch a Exception");
        }
        System.out.println("main thread end");

        Thread.setDefaultUncaughtExceptionHandler(new ThreadExceptionHandler());
        new Thread(() -> {
            throw new RuntimeException("my runtime exception");
        }).start();
        // Thread-1 caught a exception：my runtime exception

    }

    final static class ThreadExceptionHandler implements Thread.UncaughtExceptionHandler{
        @Override
        public void uncaughtException(Thread t, Throwable e) {
            System.out.println(t.getName() + " caught a exception：" + e.getMessage());
        }
    }
}
