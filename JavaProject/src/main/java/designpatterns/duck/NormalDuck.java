package designpatterns.duck;

public class NormalDuck extends Duck {

    public NormalDuck() {
        flyBehavior = new FlyWithWings();
    }

    @Override
    public void display() {
        System.out.println("normal duck");
    }
}