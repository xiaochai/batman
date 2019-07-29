package DesignPatterns.Singleton;

public class SingletonPre {
    private static SingletonPre uniqueInstance = new SingletonPre();

    private SingletonPre() {
    }

    public static SingletonPre getInstance() {
        return uniqueInstance;
    }
}

