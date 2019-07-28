package DesignPatterns.Duck;

public class FakeDuck extends Duck {
    public FakeDuck() {
        flyBehavior = new FlyNoWay();
    }

    @Override
    public void display() {
        System.out.println("fake duck");
    }
}
