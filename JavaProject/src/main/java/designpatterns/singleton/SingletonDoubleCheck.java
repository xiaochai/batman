package designpatterns.singleton;

public class SingletonDoubleCheck {
    private static volatile SingletonDoubleCheck uniqueInstance;

    private SingletonDoubleCheck() {
    }

    public static synchronized SingletonDoubleCheck getInstance() {
        if(uniqueInstance == null){
            synchronized (Singleton.class){
                if(uniqueInstance == null){
                    uniqueInstance = new SingletonDoubleCheck();
                }
            }
        }
        return uniqueInstance;
    }
}

