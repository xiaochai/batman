package designpatterns.pizza;

public abstract class Pizza {
    public abstract void prepare();

    public void bake() {
        System.out.println("bake");
    }

    public void cut() {
        System.out.println("cut");
    }

    public void box() {
        System.out.println("boxing");
    }
}
