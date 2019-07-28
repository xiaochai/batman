package DesignPatterns.Duck;

public abstract class Duck {
    public FlyBehavior flyBehavior;

    public void performFly() {
        flyBehavior.fly();
    }

    public abstract void display();
}
